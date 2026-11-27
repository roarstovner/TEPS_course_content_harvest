import { describe, expect, it } from 'vitest'
import { mergeShard, shardsFor } from './shards.js'
import { buildModel } from './model.js'

const course = (code) => ({ institution: 'x', course_code: code })
const cy = (code, year, st) => ({ institution: 'x', course_code: code, year, programme_code: st, study_type: st })
const prog = (code) => ({ institution: 'x', programme_code: code })

describe('shards', () => {
  it('loads the one shard of unsharded data, else those of the ticked study types', () => {
    expect(shardsFor({}, [])).toEqual([{ code: '', dir: '' }])
    const meta = { shards: [{ code: 'GLU', dir: 'st/glu' }, { code: 'ØA', dir: 'st/oa' }] }
    expect(shardsFor(meta, [])).toEqual([])
    expect(shardsFor(meta, ['ØA', 'NONE'])).toEqual([{ code: 'ØA', dir: 'st/oa/' }])
  })

  it('keeps the first copy of a course in several shards', () => {
    const empty = { courses: [], courseYears: [], programmes: [] }
    // B is in both shards, whole: its course-years of both types
    const glu = { courses: [course('A'), course('B')], courseYears: [cy('A', 2024, 'GLU'), cy('B', 2024, 'GLU'), cy('B', 2024, 'LEK')], programmes: [prog('GLU'), prog('LEK')] }
    const lek = { courses: [course('B'), course('C')], courseYears: [cy('B', 2024, 'GLU'), cy('B', 2024, 'LEK'), cy('C', 2025, 'LEK')], programmes: [prog('GLU'), prog('LEK')] }
    const acc = mergeShard(mergeShard(empty, glu), lek)
    expect(acc.courses.map((c) => c.course_code)).toEqual(['A', 'B', 'C'])
    expect(acc.courseYears).toHaveLength(4)
    expect(acc.programmes).toHaveLength(2)
    const m = buildModel(acc)
    expect([...m.byKey.get('x|B').years.get(2024).studyTypes]).toEqual(['GLU', 'LEK'])
    expect([m.yearMin, m.yearMax]).toEqual([2024, 2025])
  })

  it('gives an empty model the year it is given', () => {
    const m = buildModel({ courses: [], courseYears: [], year: 2026 })
    expect([m.yearMin, m.yearMax, m.yearFrom]).toEqual([2026, 2026, 2026])
  })
})
