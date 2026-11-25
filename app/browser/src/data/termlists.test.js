import { describe, expect, it } from 'vitest'
import * as yaml from 'yaml'
import { buildModel } from './model.js'
import { parseCsv } from '../download/encode.js'
import { loadLists, matrixCsv, parseTermLists, saveLists, serializeTermList, slug, termMatrix, termQuery, toTerms } from './termlists.js'

// the shape of emneplan_LK20/LK20_terms.yaml (abridged)
const LK20 = `# LK20_terms.yaml
categories:
  tverrfaglig_tema:
    label: "Tverrfaglig tema"
terms:
  # ── Kategori A
  - id: livsmestring
    label: "Livsmestring / folkehelse og livsmestring"
    label_short: "Livsmestring"
    category: tverrfaglig_tema
    strength: A
    introduced_in:
      document: "NOU 2015:8"
      year: 2015
    regex: 'folkehelse|livs(meistring|mestring)'
    nynorsk: livsmeistring
    notes: >-
      Én av tre tverrfaglige temaer.
  - id: lk20
    label: "LK20"
    regex: '\\bLK20\\b'
`

describe('term queries', () => {
  it('takes the query, else the regex, else the variants', () => {
    expect(termQuery({ query: ' a OR b ', regex: 'x' })).toBe('a OR b')
    expect(termQuery({ regex: 'd(ybde|jupne)læring', nynorsk: 'djupnelæring' })).toBe('/d(ybde|jupne)læring/')
    expect(termQuery({ variants: ['dybdelæring', 'deep learning'], nynorsk: 'djupnelæring', english: 'in-depth learning' })).toBe(
      'dybdelæring OR "deep learning" OR djupnelæring OR "in-depth learning"',
    )
    expect(termQuery({ variants: ['utforsk*', '"ikt"', '/a b/'] })).toBe('utforsk* OR "ikt" OR /a b/')
    expect(termQuery({ label: 'x' })).toBe('')
    expect(slug('Dybdelæring / deep learning')).toBe('dybdelæring_deep_learning')
  })

  it('reads the term shapes', () => {
    expect(toTerms(['ikt', 'deep learning']).map((t) => [t.id, t.query])).toEqual([
      ['ikt', 'ikt'],
      ['deep learning', '"deep learning"'],
    ])
    expect(toTerms({ dybdelæring: ['dybdelæring', 'deep learning'], ikt: '"ikt"', bk: { label: 'Bærekraft', regex: 'bærekraft' } })).toEqual([
      { id: 'dybdelæring', label: 'dybdelæring', variants: ['dybdelæring', 'deep learning'], query: 'dybdelæring OR "deep learning"' },
      { id: 'ikt', label: 'ikt', query: '"ikt"' },
      { id: 'bk', label: 'Bærekraft', regex: 'bærekraft', query: '/bærekraft/' },
    ])
    expect(toTerms([{ query: 'a' }])[0]).toMatchObject({ id: 'term1', label: 'term1' })
  })
})

describe('term list YAML', () => {
  it('reads LK20_terms.yaml with every field', async () => {
    const [l] = await parseTermLists(LK20, 'LK20_terms', yaml)
    expect(l.name).toBe('LK20_terms')
    expect(l.meta).toEqual({ categories: { tverrfaglig_tema: { label: 'Tverrfaglig tema' } } })
    expect(l.terms.map((t) => [t.id, t.query])).toEqual([
      ['livsmestring', '/folkehelse|livs(meistring|mestring)/'],
      ['lk20', '/\\bLK20\\b/'],
    ])
    expect(l.terms[0]).toMatchObject({ strength: 'A', introduced_in: { year: 2015 }, notes: 'Én av tre tverrfaglige temaer.' })
  })

  it('reads several lists of the simple shape', async () => {
    const lists = await parseTermLists('didactics:\n  dybdelæring: [dybdelæring, deep learning]\n  ikt: \'"ikt"\'\nempty: {}\n', 'x', yaml)
    expect(lists.map((l) => [l.name, l.terms.map((t) => t.query)])).toEqual([['didactics', ['dybdelæring OR "deep learning"', '"ikt"']]])
  })

  it('rejects what is not a term list', async () => {
    await expect(parseTermLists('- a\n- b\n', 'x', yaml)).rejects.toThrow(/YAML map/)
    await expect(parseTermLists('a: 1\n', 'x', yaml)).rejects.toThrow(/No terms/)
    await expect(parseTermLists('a: [', 'x', yaml)).rejects.toThrow()
  })

  it('writes a list that reads back the same, keeping the shape of an import', async () => {
    const [l] = await parseTermLists(LK20, 'LK20_terms', yaml)
    l.terms.push({ id: 'dybde', label: 'Dybdelæring', query: 'dybdelæring OR "deep learning"' })
    l.terms[1] = { ...l.terms[1], query: '"LK20" OR fagfornyelse*' } // edited: the query is written
    const text = await serializeTermList(l, yaml, new Date('2026-11-25T12:00:00Z'))
    expect(text).toMatch(/^# Term list "LK20_terms", exported from the course plan browser 2026-11-25/)
    const raw = yaml.parse(text)
    expect(raw.terms[0].query).toBeUndefined() // the regex gives it
    expect(raw.terms[0].regex).toBe('folkehelse|livs(meistring|mestring)')
    expect(raw.terms[1].query).toBe('"LK20" OR fagfornyelse*')
    const [back] = await parseTermLists(text, 'other', yaml)
    expect(back).toEqual(l)
  })
})

describe('term list storage', () => {
  const mem = () => {
    const m = new Map()
    return { getItem: (k) => m.get(k) ?? null, setItem: (k, v) => m.set(k, v) }
  }
  it('saves and loads, and survives blocked or broken storage', () => {
    const store = mem()
    const lists = { a: { terms: [{ id: 'x', label: 'x', query: 'x' }], meta: {} } }
    expect(saveLists(lists, store)).toBe(true)
    expect(loadLists(store)).toEqual(lists)
    const blocked = { getItem() { throw new Error('SecurityError') }, setItem() { throw new Error('QuotaExceeded') } }
    expect(loadLists(blocked)).toEqual({})
    expect(saveLists(lists, blocked)).toBe(false)
    store.setItem('termlists', '[1')
    expect(loadLists(store)).toEqual({})
  })
})

describe('term matrix', () => {
  const cy = (institution, course_code, year, plan_content_id = null) => ({ institution, course_code, year, study_type: 'GLU', plan_content_id })
  const model = buildModel({
    courses: [
      { institution: 'uio', course_code: 'A' },
      { institution: 'uio', course_code: 'B' },
      { institution: 'ntnu', course_code: 'N' },
    ],
    courseYears: [
      cy('uio', 'A', 2020, 'p1'),
      cy('uio', 'A', 2021, 'p1'),
      cy('uio', 'A', 2021, 'p2'), // two plans: matches when either does
      cy('uio', 'B', 2021), // no plan
      cy('ntnu', 'N', 2021, 'q1'),
    ],
  })
  const f = { inst: [], y0: null, y1: null, st: [] }
  const nums = (pts) => pts.map((p) => [p.n, p.withPlan, p.matching])

  it('counts course-years per term, overall and per institution', () => {
    const m = termMatrix(model, f, [new Set(['uio_A_p2']), new Set(['uio_A_p1', 'ntnu_N_q1']), null])
    expect(m.years).toEqual([2020, 2021])
    expect(m.institutions).toEqual(['ntnu', 'uio'])
    expect(nums(m.terms[0].all)).toEqual([[1, 1, 0], [3, 2, 1]])
    expect(nums(m.terms[1].all)).toEqual([[1, 1, 1], [3, 2, 2]])
    expect(nums(m.terms[1].inst.get('uio'))).toEqual([[1, 1, 1], [2, 1, 1]])
    expect(nums(m.terms[1].inst.get('ntnu'))).toEqual([[0, 0, 0], [1, 1, 1]])
    expect(m.terms[2]).toBeNull()
    // the filters apply
    expect(termMatrix(model, { ...f, inst: ['ntnu'], y0: 2021 }, [new Set()]).institutions).toEqual(['ntnu'])
  })

  it('exports the matrix as CSV, one row per term, institution and year', () => {
    const terms = [{ id: 'a', label: 'A', query: 'a' }, { id: 'b', label: 'B', query: 'b' }]
    const m = termMatrix(model, f, [new Set(['uio_A_p2']), null])
    const rows = parseCsv(matrixCsv('L', terms, m, 'plan'))
    expect(rows).toHaveLength(6) // (all, ntnu, uio) x 2 years, term b not run
    expect(rows[1]).toEqual({ list: 'L', term_id: 'a', term_label: 'A', query: 'a', scope: 'plan', institution: 'all', year: '2021', course_years: '3', with_plan: '2', matching: '1', share: '0.5' })
    expect(rows.find((r) => r.institution === 'ntnu' && r.year === '2020').share).toBe('')
  })
})
