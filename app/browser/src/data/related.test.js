import { expect, it } from 'vitest'
import { addPlans, buildModel } from './model.js'
import { planKey } from './keys.js'
import { planUse, sameText, yearRanges } from './related.js'

const cy = (course_code, year, programme_code, plan_content_id) => ({ institution: 'uio', course_code, year, programme_code, plan_content_id })
const model = addPlans(
  buildModel({
    courses: [{ institution: 'uio', course_code: 'A1' }, { institution: 'uio', course_code: 'B2' }, { institution: 'ntnu', course_code: 'C3' }],
    courseYears: [cy('A1', 2020, 'GLU', 'p1'), cy('A1', 2020, 'LEK', 'p1'), cy('A1', 2021, 'GLU', 'p1'), cy('A1', 2021, 'GLU', 'p2'), cy('A1', 2023, 'GLU', 'p1')],
  }),
  [
    { institution: 'uio', course_code: 'A1', plan_content_id: 'p1', year_from: 2020, year_to: 2023 },
    { institution: 'uio', course_code: 'A1', plan_content_id: 'p2', year_from: 2021, year_to: 2021 },
    { institution: 'uio', course_code: 'B2', plan_content_id: 'p1', year_from: 2022, year_to: 2022 },
    { institution: 'ntnu', course_code: 'C3', plan_content_id: 'p1', year_from: 2022, year_to: 2022 },
  ],
)
const programmes = [
  { institution: 'uio', programme_code: 'LEK', programme_name: 'Lektor', study_type: 'INTMASTER' },
  { institution: 'uio', programme_code: 'GLU', programme_name: 'Grunnskole', study_type: 'GLU5-10' },
  { institution: 'ntnu', programme_code: 'GLU', programme_name: 'Other', study_type: 'X' },
]
const a1 = model.byKey.get('uio|A1')

it('lists the years and programmes that use a plan', () => {
  const key = planKey({ institution: 'uio', course_code: 'A1', plan_content_id: 'p1' })
  expect(planUse(a1, key, programmes)).toEqual({
    years: [2020, 2021, 2023],
    programmes: [
      { programme_code: 'GLU', programme_name: 'Grunnskole', study_type: 'GLU5-10', years: [2020, 2021, 2023] },
      { programme_code: 'LEK', programme_name: 'Lektor', study_type: 'INTMASTER', years: [2020] },
    ],
  })
})

it('finds other courses of the institution with the same text', () => {
  const r = sameText(model, a1.versions.find((v) => v.plan_content_id === 'p1'))
  expect(r.map((x) => x.course.course_code)).toEqual(['B2'])
  expect(sameText(model, a1.versions.find((v) => v.plan_content_id === 'p2'))).toEqual([])
})

it('writes years as ranges', () => {
  expect(yearRanges([2019, 2020, 2021, 2023])).toBe('2019–2021, 2023')
  expect(yearRanges([])).toBe('')
})
