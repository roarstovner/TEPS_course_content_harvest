import { describe, expect, it } from 'vitest'
import { addPlans, buildModel, filterView } from './model.js'
import { acrossPlans, defaultOther, findCourses, planRef, resolveRef } from './compare.js'

const courses = [
  { institution: 'uio', course_code: 'MAT1', course_name: 'Matematikk 1' },
  { institution: 'uio', course_code: 'NOR1', course_name: 'Norsk 1' },
  { institution: 'ntnu', course_code: 'MA:1', course_name: 'Matematikk 1 for GLU' },
  { institution: 'ntnu', course_code: 'X', course_name: 'No plan' },
]
const plan = (institution, course_code, plan_content_id, year_from, year_to) => ({ institution, course_code, plan_content_id, year_from, year_to })
const plans = [
  plan('uio', 'MAT1', 'aaaaaaaaaaaa01', 2018, 2021),
  plan('uio', 'MAT1', 'bbbbbbbbbbbb02', 2022, 2024),
  plan('uio', 'NOR1', 'cccccccccccc03', 2020, 2024),
  plan('ntnu', 'MA:1', 'dddddddddddd04', 2023, 2024),
]
const courseYears = plans.flatMap((p) => [p.year_from, p.year_to].map((year) => ({ ...p, year, study_type: 'GLU' })))
const model = addPlans(buildModel({ courses, courseYears }), plans)
const mat = model.byKey.get('uio|MAT1')

describe('compare', () => {
  it('round-trips a plan reference, falling back to the latest version', () => {
    const ref = planRef(mat, mat.versions[1])
    expect(ref).toBe('uio|MAT1@aaaaaaaaaaaa')
    expect(resolveRef(model, ref)).toEqual({ course: mat, version: mat.versions[1] })
    const odd = model.byKey.get('ntnu|MA:1')
    expect(resolveRef(model, planRef(odd, odd.versions[0])).course).toBe(odd) // a ':' in the course code
    expect(resolveRef(model, 'uio|MAT1@gone').version).toBe(mat.versions[0])
    expect(resolveRef(model, 'uio|NONE@x')).toBeNull()
    expect(resolveRef(model, 'ntnu|X@x')).toBeNull() // no plan
    expect(resolveRef(model, null)).toBeNull()
  })

  it('compares with the previous version of the same course by default', () => {
    expect(defaultOther({ course: mat, version: mat.versions[0] }).version).toBe(mat.versions[1])
    expect(defaultOther({ course: mat, version: mat.versions[1] }).version).toBe(mat.versions[0])
    const nor = model.byKey.get('uio|NOR1')
    expect(defaultOther({ course: nor, version: nor.versions[0] })).toBeNull()
  })

  it('finds courses with a plan by every word', () => {
    expect(findCourses(model, 'matematikk 1').map((c) => c.key)).toEqual(['ntnu|MA:1', 'uio|MAT1'])
    expect(findCourses(model, 'UIO mat').map((c) => c.key)).toEqual(['uio|MAT1'])
    expect(findCourses(model, 'no plan')).toEqual([])
    expect(findCourses(model, '  ')).toEqual([])
    expect(findCourses(model, 'matematikk', 1)).toHaveLength(1)
  })

  it('lines up the plans in force in a year, a column per institution', () => {
    const rows = filterView(model, { inst: [], y0: null, y1: null, st: [] }, null).rows
    const a = acrossPlans(rows, 2024)
    expect(a.total).toBe(3)
    expect(a.columns.map((c) => [c.institution, c.plans.map((p) => p.course.course_code)])).toEqual([
      ['ntnu', ['MA:1']],
      ['uio', ['MAT1', 'NOR1']],
    ])
    expect(acrossPlans(rows, 2019).columns[0].plans[0].version.plan_content_id).toBe('aaaaaaaaaaaa01')
    const capped = acrossPlans(rows, 2024, 2)
    expect([capped.total, capped.columns.flatMap((c) => c.plans).length]).toEqual([3, 2])
  })
})
