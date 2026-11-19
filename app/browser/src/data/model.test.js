import { describe, expect, it } from 'vitest'
import { addPlans, buildModel, filterView, pickVersion, yearStrip } from './model.js'
import { planKey, versionId } from './keys.js'

// Two courses at uio, one at ntnu. A course-year has a row per programme and
// plan, as in course_years.parquet.
const courses = [
  { institution: 'uio', course_code: 'B2', course_name: 'Bee' },
  { institution: 'ntnu', course_code: 'N1', course_name: 'En' },
  { institution: 'uio', course_code: 'A1', course_name: 'Ay' },
]
const cy = (institution, course_code, year, study_type, plan_content_id = null) => ({
  institution,
  course_code,
  year,
  study_type,
  plan_content_id,
})
const courseYears = [
  cy('uio', 'A1', 2020, 'GLU', 'p1'),
  cy('uio', 'A1', 2020, 'LEKTOR', 'p1'), // same course-year, second programme
  cy('uio', 'A1', 2021, 'GLU', 'p1'),
  cy('uio', 'A1', 2021, 'GLU', 'p2'), // a second plan that year
  cy('uio', 'A1', 2022, 'GLU', 'p2'),
  cy('uio', 'B2', 2021, 'LEKTOR'), // no plan
  cy('uio', 'B2', 2022, 'LEKTOR', 'p1'), // same id as A1's p1, other course
  cy('ntnu', 'N1', 2022, 'GLU', 'q1'),
]
const plans = [
  { institution: 'uio', course_code: 'A1', plan_content_id: 'p1', year_from: 2020, year_to: 2021 },
  { institution: 'uio', course_code: 'A1', plan_content_id: 'p2', year_from: 2021, year_to: 2022 },
  { institution: 'uio', course_code: 'B2', plan_content_id: 'p1', year_from: 2022, year_to: 2022 },
  { institution: 'ntnu', course_code: 'N1', plan_content_id: 'q1', year_from: 2022, year_to: 2022 },
]
const make = () => addPlans(buildModel({ courses, courseYears }), plans)
const all = { inst: [], y0: null, y1: null, st: [] }
const codes = (v) => v.rows.map((r) => r.course.key)

describe('buildModel', () => {
  it('sorts courses and collapses rows to course-years', () => {
    const m = make()
    expect(m.courses.map((c) => c.key)).toEqual(['ntnu|N1', 'uio|A1', 'uio|B2'])
    expect(m.courseYears).toHaveLength(6)
    expect([m.yearMin, m.yearMax]).toEqual([2020, 2022])
    const a2021 = m.byKey.get('uio|A1').years.get(2021)
    expect([...a2021.plans]).toEqual(['uio_A1_p1', 'uio_A1_p2'])
  })
  it('uses plan_id when the data have it', () => {
    expect(planKey({ plan_id: 'x_1_abc', institution: 'x', course_code: '1', plan_content_id: 'abc' })).toBe('x_1_abc')
  })
  it('keeps plan versions apart by course (the id alone is not unique)', () => {
    const m = make()
    expect(m.byKey.get('uio|A1').versions.map((v) => v.plan_content_id)).toEqual(['p2', 'p1'])
    expect(m.byKey.get('uio|B2').versions.map((v) => v.key)).toEqual(['uio_B2_p1'])
  })
})

describe('filterView', () => {
  it('without a query lists every course in the filter and counts course-years', () => {
    const v = filterView(make(), all, null)
    expect(codes(v)).toEqual(['ntnu|N1', 'uio|A1', 'uio|B2'])
    expect(v.counts).toEqual({ courses: 3, versions: 4, courseYears: 6, withPlan: 5, matching: 0 })
  })
  it('filters by institution, years and study type; instCounts ignore the institution filter', () => {
    const m = make()
    const v = filterView(m, { ...all, inst: ['uio'], y0: 2021, y1: 2021 }, null)
    expect(codes(v)).toEqual(['uio|A1', 'uio|B2'])
    expect(v.counts).toMatchObject({ courseYears: 2, withPlan: 1, versions: 2 })
    expect(Object.fromEntries(v.instCounts)).toEqual({ uio: 2 })
    const lektor = filterView(m, { ...all, st: ['LEKTOR'] }, null)
    expect(codes(lektor)).toEqual(['uio|A1', 'uio|B2'])
    expect(lektor.counts.courseYears).toBe(3)
  })
  it('a course matches when a matching version overlaps the years', () => {
    const m = make()
    const matches = new Map([
      ['uio_A1_p1', [2, 1]],
      ['uio_B2_p1', [5, 0]],
    ])
    const v = filterView(m, all, matches)
    expect(codes(v)).toEqual(['uio|A1', 'uio|B2'])
    expect(v.rows[0].hits).toBe(3)
    expect(v.counts).toEqual({ courses: 2, versions: 2, courseYears: 6, withPlan: 5, matching: 3 })
    // p1 of A1 ends in 2021: not in 2022
    expect(codes(filterView(m, { ...all, y0: 2022 }, matches))).toEqual(['uio|B2'])
  })
})

describe('yearStrip and pickVersion', () => {
  it('marks years by matching version, plan, no plan and not taught', () => {
    const m = make()
    const b = m.byKey.get('uio|B2')
    expect(yearStrip(b, 2020, 2022, new Set(['uio_B2_p1'])).map((c) => c.state)).toEqual(['', 'none', 'match'])
    expect(yearStrip(b, 2020, 2022, null).map((c) => c.state)).toEqual(['', 'none', 'plan'])
  })
  it('picks the requested, else the latest matching, else the latest version', () => {
    const m = make()
    const a = m.byKey.get('uio|A1')
    const row = { versions: [a.versions[1]] }
    expect(pickVersion(a, row, versionId(a.versions[0]), versionId).plan_content_id).toBe('p2')
    expect(pickVersion(a, row, null, versionId).plan_content_id).toBe('p1')
    expect(pickVersion(a, null, 'nope', versionId).plan_content_id).toBe('p2')
    expect(planKey(a.versions[0])).toBe('uio_A1_p2')
  })
})
