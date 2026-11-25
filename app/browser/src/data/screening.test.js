import { describe, expect, it } from 'vitest'
import { hitId, loadTags, sampleIndices, saveTags, screeningCsv, seededRandom, storeKey } from './screening.js'
import { parseCsv } from '../download/encode.js'

describe('sampleIndices', () => {
  it('is deterministic for a seed and differs between seeds', () => {
    expect(sampleIndices(1000, 20, '123456')).toEqual(sampleIndices(1000, 20, '123456'))
    expect(sampleIndices(1000, 20, '123456')).not.toEqual(sampleIndices(1000, 20, '654321'))
    expect(seededRandom('x')()).toBe(seededRandom('x')())
  })

  it('draws without replacement, sorted, within range', () => {
    const s = sampleIndices(50, 30, 'a')
    expect(s).toHaveLength(30)
    expect(new Set(s).size).toBe(30)
    expect(s).toEqual([...s].sort((a, b) => a - b))
    expect(s.every((i) => Number.isInteger(i) && i >= 0 && i < 50)).toBe(true)
  })

  it('takes everything when n is at least the total', () => {
    expect(sampleIndices(5, 10, 'a')).toEqual([0, 1, 2, 3, 4])
    expect(sampleIndices(0, 10, 'a')).toEqual([])
  })

  it('includes every index equally often over many seeds', () => {
    const total = 20
    const n = 5
    const runs = 20000
    const counts = new Array(total).fill(0)
    for (let r = 0; r < runs; r++) for (const i of sampleIndices(total, n, `seed${r}`)) counts[i]++
    const expected = (runs * n) / total // 5000
    // chi-square with 19 degrees of freedom: 43.8 is the 0.1% critical value
    const chi2 = counts.reduce((s, c) => s + (c - expected) ** 2 / expected, 0)
    expect(chi2).toBeLessThan(43.8)
    // pairs too: index 0 and 1 together in n(n-1)/(total(total-1)) of the samples
    let both = 0
    for (let r = 0; r < runs; r++) {
      const s = sampleIndices(total, n, `seed${r}`)
      if (s[0] === 0 && s[1] === 1) both++
    }
    expect(Math.abs(both / runs - (n * (n - 1)) / (total * (total - 1)))).toBeLessThan(0.01)
  })
})

describe('tags', () => {
  const memory = () => {
    const m = new Map()
    return { getItem: (k) => m.get(k) ?? null, setItem: (k, v) => m.set(k, v), removeItem: (k) => m.delete(k), m }
  }

  it('keys differ by frame, query, scope and unticked forms, not by their order', () => {
    const a = { q: 'ikt', scope: 'plan', n: 10, excl: {} }
    expect(storeKey('teacher_ed', a)).toBe(storeKey('teacher_ed', { ...a, n: 5 }))
    expect(storeKey('teacher_ed', a)).not.toBe(storeKey('all', a))
    expect(storeKey('teacher_ed', a)).not.toBe(storeKey('teacher_ed', { ...a, q: 'ikt*' }))
    expect(storeKey('teacher_ed', a)).not.toBe(storeKey('teacher_ed', { ...a, scope: 'sentence' }))
    expect(storeKey('teacher_ed', a)).not.toBe(storeKey('teacher_ed', { ...a, excl: { ikt: ['x'] } }))
    expect(storeKey('f', { ...a, excl: { b: ['y', 'x'], a: ['z'] } })).toBe(storeKey('f', { ...a, excl: { a: ['z'], b: ['x', 'y'] } }))
  })

  it('round-trip through storage and survive a broken or missing one', () => {
    const st = memory()
    expect(saveTags('k', { h: { tag: 'include', note: '' } }, st)).toBe(true)
    expect(loadTags('k', st)).toEqual({ h: { tag: 'include', note: '' } })
    saveTags('k', {}, st)
    expect(st.m.has('k')).toBe(false)
    const broken = { getItem: () => '{', setItem: () => { throw new Error('quota') }, removeItem: () => {} }
    expect(loadTags('k', broken)).toEqual({})
    expect(saveTags('k', { h: {} }, broken)).toBe(false)
    expect(loadTags('k', undefined)).toEqual({})
  })
})

describe('screeningCsv', () => {
  it('writes every hit of the sample with its tag and note', () => {
    const hit = (plan_id, position, before) => ({
      plan_id, institution: 'uio', course_code: 'EDU1000', course_name: 'Pedagogikk', year_from: 2020, year_to: 2024,
      section: null, term: 'ikt', position, before, match: 'IKT', after: '-basert, "digital"',
    })
    const hits = [hit('uio_EDU1000_abc', 10, 'bruk av\n'), hit('uio_EDU1000_abc', 99, 'om')]
    const tags = { [hitId(hits[0])]: { tag: 'exclude', note: 'navn, ikke tema' } }
    const csv = screeningCsv(hits, tags, { q: '"ikt"', seed: '42' })
    expect(csv.startsWith('﻿sample_rank,institution,course_code')).toBe(true)
    const rows = parseCsv(csv)
    expect(rows).toHaveLength(2)
    expect(rows[0]).toMatchObject({
      sample_rank: '1', plan_id: 'uio_EDU1000_abc', year_from: '2020', section: '', position: '10',
      before: 'bruk av\n', after: '-basert, "digital"', tag: 'exclude', note: 'navn, ikke tema', query: '"ikt"', seed: '42',
    })
    expect(rows[1]).toMatchObject({ sample_rank: '2', tag: '', note: '' })
  })
})
