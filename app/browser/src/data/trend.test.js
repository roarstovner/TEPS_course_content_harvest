import { describe, expect, it } from 'vitest'
import { buildModel } from './model.js'
import { MAX_SERIES, OTHERS, coverage, trend } from './trend.js'

// A course-year has a row per programme and plan, as in course_years.parquet.
const courses = [
  { institution: 'uio', course_code: 'A1', level_code: 'HN' },
  { institution: 'uio', course_code: 'B2' },
  { institution: 'ntnu', course_code: 'N1' },
]
const cy = (institution, course_code, year, study_type, plan_content_id = null, no_plan_reason = null) => ({
  institution,
  course_code,
  year,
  study_type,
  plan_content_id,
  no_plan_reason,
})
const courseYears = [
  cy('uio', 'A1', 2020, 'GLU', 'p1'),
  cy('uio', 'A1', 2020, 'LEKTOR', 'p1'), // same course-year, second programme
  cy('uio', 'A1', 2021, 'GLU', 'p1'),
  cy('uio', 'A1', 2021, 'GLU', 'p2'), // a second plan that year
  cy('uio', 'A1', 2022, 'GLU', 'p2'),
  cy('uio', 'B2', 2021, 'LEKTOR', null, '404'),
  cy('uio', 'B2', 2022, 'LEKTOR', 'p1'), // same id as A1's p1, other course
  cy('ntnu', 'N1', 2018, 'GLU'), // before the first plan
  cy('ntnu', 'N1', 2021, 'GLU'), // no plan, no reason recorded
  cy('ntnu', 'N1', 2022, 'GLU', 'q1'),
]
const model = buildModel({ courses, courseYears })
const all = { inst: [], y0: null, y1: null, st: [] }
const m = (...keys) => new Map(keys.map((k) => [k, [1]]))
// points as [n, withPlan, matching]
const nums = (pts) => pts.map((p) => [p.n, p.withPlan, p.matching])
const byKey = (t) => Object.fromEntries(t.series.map((s) => [s.key, nums(s.points)]))

describe('trend', () => {
  it('starts at the first year with a plan and counts each course-year once', () => {
    expect([model.yearMin, model.yearFrom]).toEqual([2018, 2020])
    const t = trend(model, all, m('uio_A1_p2'))
    expect(t.years).toEqual([2020, 2021, 2022])
    expect(nums(t.total)).toEqual([
      [1, 1, 0],
      [3, 1, 1], // A1 matches through its second plan
      [3, 3, 1],
    ])
    expect(t.series).toEqual([])
    expect(trend(model, { ...all, y0: 2018 }, null).years[0]).toBe(2018)
  })

  it('matches a plan by course, not by its text id alone', () => {
    // B2's plan shares A1's p1 id; only A1's version matches
    expect(nums(trend(model, all, m('uio_A1_p1')).total).map((p) => p[2])).toEqual([1, 1, 0])
  })

  it('counts nothing as matching without a query', () => {
    expect(nums(trend(model, all, null).total).map((p) => p[2])).toEqual([0, 0, 0])
  })

  it('groups by institution and by study type', () => {
    const q = m('uio_A1_p2')
    expect(byKey(trend(model, all, q, 'inst'))).toEqual({
      ntnu: [[0, 0, 0], [1, 0, 0], [1, 1, 0]],
      uio: [[1, 1, 0], [2, 1, 1], [2, 2, 1]],
    })
    expect(byKey(trend(model, all, q, 'st'))).toEqual({
      GLU: [[1, 1, 0], [2, 1, 1], [2, 2, 1]],
      LEKTOR: [[1, 1, 0], [1, 0, 0], [1, 1, 0]],
    })
  })

  it('applies the institution and study type filters', () => {
    expect(byKey(trend(model, { ...all, st: ['LEKTOR'] }, m('uio_A1_p2'), 'st'))).toEqual({
      LEKTOR: [[1, 1, 0], [1, 0, 0], [1, 1, 0]],
    })
    expect(nums(trend(model, { ...all, inst: ['ntnu'] }, null).total)).toEqual([
      [0, 0, 0],
      [1, 0, 0],
      [1, 1, 0],
    ])
    // a course filter (level): A1 only
    expect(nums(trend(model, { ...all, lvl: ['HN'] }, null).total).map((p) => p[0])).toEqual([1, 1, 1])
  })

  it(`folds groups past ${MAX_SERIES} into others, each course-year once`, () => {
    const n = MAX_SERIES + 2
    const cs = Array.from({ length: n }, (_, i) => ({ institution: `i${i}`, course_code: 'C' }))
    // institution i gets i + 1 course-years with a plan (2001..)
    const rows = cs.flatMap((c, i) => Array.from({ length: i + 1 }, (_, j) => cy(c.institution, 'C', 2001 + j, 'X', 'p')))
    const t = trend(buildModel({ courses: cs, courseYears: rows }), all, null, 'inst')
    expect(t.series).toHaveLength(MAX_SERIES)
    const others = t.series.at(-1)
    expect([others.key, others.other]).toEqual([OTHERS, true])
    // the three smallest (i0, i1, i2) in others: 3 course-years in 2001
    expect(others.points[0].n).toBe(3)
    expect(t.series.map((s) => s.key)).not.toContain('i0')
  })
})

describe('coverage', () => {
  it('counts course-years, those with a plan and the no-plan reasons per institution', () => {
    expect(coverage(model, all)).toEqual([
      { institution: 'ntnu', n: 2, withPlan: 1, reasons: [[null, 1]] },
      { institution: 'uio', n: 5, withPlan: 4, reasons: [['404', 1]] },
    ])
    expect(coverage(model, { ...all, y0: 2018 })[0].n).toBe(3)
  })
})
