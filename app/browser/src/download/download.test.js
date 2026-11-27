import { describe, expect, it } from 'vitest'
import { strFromU8, unzipSync } from 'fflate'
import { parquetReadObjects } from 'hyparquet'
import { addPlans, buildModel, filterView } from '../data/model.js'
import { selection, unitRows } from './table.js'
import { EXCEL_MAX, parseCsv, toCsv, toParquet, toXlsx, truncateForExcel } from './encode.js'
import { buildDownload, manifest, qdaDocs, variables, zipName } from './bundle.js'

// Two courses at uio, one at ntnu (as in model.test.js), with programmes,
// students and a no-plan reason.
const courses = [
  { institution: 'uio', course_code: 'A1', course_name: 'Ay', credits: 10 },
  { institution: 'uio', course_code: 'B2', course_name: 'Bee', credits: 15 },
  { institution: 'ntnu', course_code: 'N1', course_name: 'En', credits: 7.5 },
]
const cy = (institution, course_code, year, study_type, plan_content_id = null, extra = {}) => ({
  institution,
  course_code,
  year,
  study_type,
  programme_code: `${study_type}-P`,
  plan_content_id,
  ...extra,
})
const courseYears = [
  cy('uio', 'A1', 2020, 'GLU', 'p1', { had_students: true }),
  cy('uio', 'A1', 2020, 'LEKTOR', 'p1'),
  cy('uio', 'A1', 2021, 'GLU', 'p2'),
  cy('uio', 'B2', 2021, 'LEKTOR', null, { no_plan_reason: '404' }),
  cy('uio', 'B2', 2022, 'LEKTOR', 'p1'),
  cy('ntnu', 'N1', 2022, 'GLU', 'q1'),
]
const plans = [
  { institution: 'uio', course_code: 'A1', plan_content_id: 'p1aaaaaaaaaa', year_from: 2020, year_to: 2020, source_url: 'u1' },
  { institution: 'uio', course_code: 'A1', plan_content_id: 'p2aaaaaaaaaa', year_from: 2021, year_to: 2021 },
  { institution: 'uio', course_code: 'B2', plan_content_id: 'p1aaaaaaaaaa', year_from: 2022, year_to: 2022 },
  { institution: 'ntnu', course_code: 'N1', plan_content_id: 'q1aaaaaaaaaa', year_from: 2022, year_to: 2022 },
]
for (const r of courseYears) if (r.plan_content_id) r.plan_content_id += 'aaaaaaaaaa'
const model = addPlans(buildModel({ courses, courseYears }), plans)
const all = { inst: [], y0: null, y1: null, st: [] }
const names = { uio: 'Universitetet i Oslo' }
// a query that matched A1's p2 (3 hits in two terms) and B2's plan
const matches = new Map([
  ['uio_A1_p2aaaaaaaa', [2, 1]],
  ['uio_B2_p1aaaaaaaa', [1, 0]],
])
const selOf = (f = all, m = matches) => selection(model, filterView(model, f, m), f, m, names)

describe('selection', () => {
  it('without a query: every course, course-year and version in the filter', () => {
    const s = selOf(all, null)
    expect(s.courses.map((c) => c.course_code)).toEqual(['N1', 'A1', 'B2'])
    expect(s.courseYears).toHaveLength(5)
    expect(s.plans).toHaveLength(4)
    expect(s.courses[1]).toMatchObject({ institution_name: 'Universitetet i Oslo', course_years: 2, plan_versions: 2, hits: null })
    expect(s.courseYears.find((r) => r.course_code === 'A1' && r.year === 2020)).toMatchObject({
      study_types: 'GLU; LEKTOR',
      programme_codes: 'GLU-P; LEKTOR-P',
      had_students: true,
      has_plan: true,
      matching: null,
    })
    expect(s.courseYears.find((r) => r.course_code === 'B2' && r.year === 2021)).toMatchObject({ has_plan: false, no_plan_reason: '404' })
  })
  it('with a query: matching courses and versions, hits and matching course-years', () => {
    const s = selOf()
    expect(s.courses.map((c) => c.course_code)).toEqual(['A1', 'B2'])
    expect(s.plans.map((p) => p.plan_id)).toEqual(['uio_A1_p2aaaaaaaa', 'uio_B2_p1aaaaaaaa'])
    expect(s.courses[0]).toMatchObject({ hits: 3, plan_versions: 1, course_years: 2, course_years_matching: 1 })
    expect(s.courseYears.filter((r) => r.matching).map((r) => `${r.course_code} ${r.year}`)).toEqual(['A1 2021', 'B2 2022'])
    // keys of the course-year units: every plan of the selected course-years
    expect(s.planKeys.map((k) => k.plan_id).sort()).toEqual(['uio_A1_p1aaaaaaaa', 'uio_A1_p2aaaaaaaa', 'uio_B2_p1aaaaaaaa'])
  })
  it('follows the study type filter', () => {
    const s = selOf({ ...all, st: ['LEKTOR'] }, null)
    expect(s.courseYears.map((r) => `${r.course_code} ${r.year}`)).toEqual(['A1 2020', 'B2 2021', 'B2 2022'])
    expect(s.plans.find((p) => p.plan_id === 'uio_A1_p1aaaaaaaa').study_types).toBe('GLU; LEKTOR') // all types of the course-years in the filter
  })
})

describe('unitRows', () => {
  const text = {
    plan: (k) => `text of ${k}`,
    sections: () => ({ reading_list: 'books', learning_outcomes: 'know' }),
    sectionHits: () => ({ learning_outcomes: 2 }),
    hits: () => [{ term: 'lese*', source: '', start: 4, before: 'a ', match: 'lese', after: ' b' }],
  }
  it('adds the text of plans, sections in display order and hits', () => {
    const s = selOf()
    expect(unitRows('plan', s, text)[0].course_plan).toBe('text of uio_A1_p2aaaaaaaa')
    const secs = unitRows('section', s, text, ['learning_outcomes', 'reading_list'])
    expect(secs.map((r) => [r.section, r.hits])).toEqual([
      ['learning_outcomes', 2],
      ['reading_list', 0],
      ['learning_outcomes', 2],
      ['reading_list', 0],
    ])
    expect(unitRows('hit', s, text)[0]).toMatchObject({ term: 'lese*', section: null, position: 4, match: 'lese' })
  })
})

describe('CSV', () => {
  it('writes RFC 4180 with a BOM and CRLF, keeping line breaks in quoted cells', () => {
    const s = toCsv(['a', 'b', 'c'], [{ a: 'x,y', b: 'say "hi"', c: 'l1\nl2' }, { a: null, b: true, c: 2.5 }])
    expect(s).toBe('﻿a,b,c\r\n"x,y","say ""hi""","l1\nl2"\r\n,TRUE,2.5\r\n')
    expect(parseCsv(s)).toEqual([
      { a: 'x,y', b: 'say "hi"', c: 'l1\nl2' },
      { a: '', b: 'TRUE', c: '2.5' },
    ])
  })
})

describe('XLSX', () => {
  it('cuts cells over the limit, keeping surrogate pairs whole, and flags the row', () => {
    const long = 'a'.repeat(EXCEL_MAX - 1) + '😀b'
    const t = truncateForExcel(['t'], [{ t: long }, { t: 'short' }], true)
    expect(t.cols).toEqual(['t', 'truncated'])
    expect(t.truncated).toBe(1)
    expect(t.rows[0].t).toHaveLength(EXCEL_MAX - 1)
    expect(t.rows.map((r) => r.truncated)).toEqual([true, false])
    expect(truncateForExcel(['t'], [{ t: 'x' }], false).cols).toEqual(['t'])
  })
  it('writes a sheet with inline strings, numbers and booleans', async () => {
    const z = unzipSync(await toXlsx(['name', 'n', 'ok'], [{ name: 'a & <b>\u0001\nc', n: 3, ok: false }]))
    expect(Object.keys(z)).toContain('[Content_Types].xml')
    const ws = strFromU8(z['xl/worksheets/sheet1.xml'])
    expect(ws).toContain('<c r="A2" t="inlineStr"><is><t xml:space="preserve">a &amp; &lt;b&gt;\nc</t></is></c>')
    expect(ws).toContain('<c r="B2"><v>3</v></c><c r="C2" t="b"><v>0</v></c>')
    expect(ws).toContain('<c r="A1" t="inlineStr" s="1">')
  })
})

describe('Parquet', () => {
  it('round-trips with hyparquet, with types and nulls', async () => {
    const buf = await toParquet(['s', 'i', 'd', 'b'], [{ s: 'æ', i: 1, d: 1.5, b: true }, { s: null, i: null, d: 2, b: false }], {
      s: 'character',
      i: 'integer',
      d: 'double',
      b: 'logical',
    })
    expect(await parquetReadObjects({ file: buf })).toEqual([
      { s: 'æ', i: 1, d: 1.5, b: true },
      { s: null, i: null, d: 2, b: false },
    ])
  })
})

describe('bundle', () => {
  const dictionary = 'file,variable,old_name,type,source,description\ncourse_plans,plan_id,plan_id,character,derived,"Plan id, unique."\n'
  const ctx = {
    q: 'lese*',
    scope: 'plan',
    n: 10,
    excl: { 'lese*': ['leseren'] },
    filters: { inst: ['uio'], y0: 2020, y1: 2022, st: [] },
    url: 'https://example.org/?q=lese*',
    meta: { version: 'teacher_ed processed 2026-11-16T00:30', frame: 'teacher_ed', built_at: 'x' },
    date: new Date('2026-11-19T14:30:00Z'),
  }
  it('describes columns from the dictionary and the browser', () => {
    const v = variables(['plan_id', 'source_url', 'unknown'], dictionary)
    expect(v[0]).toEqual({ variable: 'plan_id', type: 'character', source: 'course_plans (release)', description: 'Plan id, unique.' })
    expect(v[1].source).toBe('browser')
    expect(v[2].description).toBe('')
  })
  it('names files after the data version, unit and time', () => {
    expect(zipName(ctx.meta.version, 'plans', ctx.date)).toBe('course-plans_teacher_ed-processed-2026-11-16T00-30_plans_2026-11-19_1430Z.zip')
    const docs = qdaDocs(
      [
        { institution: 'uio', course_code: 'A/1', year_from: 2020, year_to: 2021, plan_content_id: 'abcdef12', plan_id: 'k1', course_plan: 'T' },
        { institution: 'uio', course_code: 'B', year_from: 2020, year_to: 2020, plan_content_id: 'aaaaaa11', plan_id: 'k2' },
        { institution: 'uio', course_code: 'B', year_from: 2020, year_to: 2020, plan_content_id: 'bbbbbb22', plan_id: 'k3' },
      ],
      'v1',
    )
    expect(docs.map((d) => d.name)).toEqual(['uio_A-1_2020-2021', 'uio_B_2020_aaaaaa', 'uio_B_2020_bbbbbb'])
    expect(docs[0].text).toMatch(/^﻿A\/1\nuio \(uio\)\nYears in force: 2020-2021\nPlan id: k1\nData version: v1\n\nT\n$/)
  })
  it('writes the query, filters and counts to the manifest', () => {
    const m = manifest(ctx, 'plan', 'csv', { rows: 2 }, ['plans.csv'])
    expect(m).toMatchObject({ unit: 'plan', format: 'csv', query: { q: 'lese*', scope: 'plan', excluded_forms: { 'lese*': ['leseren'] } } })
    expect(m.query.n).toBeUndefined()
    expect(m.filters).toEqual({ institutions: ['uio'], years: [2020, 2022], study_types: [], subjects: [], nus_groups: [], levels: [], credits: [], languages: [], sections: [] })
    expect(m.data.version).toBe(ctx.meta.version)
  })
  it('zips the table with its manifest, variables, keys and rebuild code', async () => {
    const sel = selOf()
    const rows = unitRows('plan', sel, { plan: () => 'x'.repeat(EXCEL_MAX + 5) })
    const out = await buildDownload({ unit: 'plan', format: 'xlsx', rows, sel, ctx, dictionary })
    expect(out.truncated).toBe(2)
    const z = unzipSync(out.bytes)
    expect(Object.keys(z).sort()).toEqual(['keys.csv', 'manifest.json', 'plans.xlsx', 'rebuild.R', 'rebuild.py', 'variables.csv'])
    const m = JSON.parse(strFromU8(z['manifest.json']))
    expect(m.counts).toEqual({ rows: 2, courses: 2, plan_versions: 2, truncated_in_xlsx: 2 })
    expect(parseCsv(strFromU8(z['keys.csv'])).map((k) => k.plan_id)).toEqual(['uio_A1_p2aaaaaaaa', 'uio_B2_p1aaaaaaaa'])
    expect(parseCsv(strFromU8(z['variables.csv'])).at(-1)).toMatchObject({ variable: 'truncated', type: 'logical' })
    expect(strFromU8(z['rebuild.R'])).toContain('result <- plans')
  })
  it('writes course-years and their plans for the course units', async () => {
    const sel = selOf()
    const z = unzipSync((await buildDownload({ unit: 'course_year', format: 'csv', rows: sel.courseYears, sel, ctx, dictionary })).bytes)
    expect(parseCsv(strFromU8(z['course_year_keys.csv']))).toHaveLength(sel.courseYears.length)
    expect(parseCsv(strFromU8(z['keys.csv']))).toHaveLength(3)
    expect(strFromU8(z['rebuild.py'])).toContain('result = offerings[["institution", "course_code", "year"]].drop_duplicates()')
  })
  it('makes QDA files: one text per plan and a classification sheet', async () => {
    const sel = selOf()
    const rows = unitRows('plan', sel, { plan: (k) => `text ${k}` })
    const z = unzipSync((await buildDownload({ unit: 'plan', format: 'qda', rows, sel, ctx, dictionary })).bytes)
    expect(Object.keys(z).filter((f) => f.startsWith('texts/'))).toEqual(['texts/uio_A1_2021.txt', 'texts/uio_B2_2022.txt'])
    const sheet = parseCsv(strFromU8(z['classification.csv']))
    expect(sheet.map((r) => r.document)).toEqual(['uio_A1_2021', 'uio_B2_2022'])
    expect(sheet[0].course_plan).toBeUndefined()
    expect(z['classification.xlsx']).toBeDefined()
  })
})
