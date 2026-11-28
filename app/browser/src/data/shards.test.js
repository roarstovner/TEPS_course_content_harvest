import { describe, expect, it } from 'vitest'
import { WARN_BYTES, mergeShard, planLoad, shardsFor } from './shards.js'
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

  it('holds a selection that passes a size step until it is accepted', () => {
    const M = WARN_BYTES / 30 // 1 MB at the 30 MB threshold
    const meta = { shards: ['A', 'B', 'C', 'D', 'E'].map((code, i) => ({ code, dir: `st/${code.toLowerCase()}`, bytes: [10, 15, 10, 3, 25][i] * M })) }
    const codes = (r) => [r.load.map((x) => x.code), r.hold.map((x) => x.code)]
    // under the threshold: loads at once
    expect(codes(planLoad(meta, ['A', 'B'], []))).toEqual([['A', 'B'], []])
    // crossing it: holds every new shard; those loaded count toward the total
    const r = planLoad(meta, ['A', 'B', 'C'], ['A', 'B'])
    expect(codes(r)).toEqual([[], ['C']])
    expect([r.bytes, r.more]).toEqual([35 * M, 10 * M])
    expect(codes(planLoad(meta, ['C', 'D'], ['A', 'B']))).toEqual([[], ['C', 'D']])
    // accepted at 35 MB: a small addition (38 MB) loads
    expect(codes(planLoad(meta, ['A', 'B', 'C', 'D'], ['A', 'B', 'C'], 35 * M))).toEqual([['D'], []])
    // the next step (63 MB) asks again
    expect(codes(planLoad(meta, ['E'], ['A', 'B', 'C', 'D'], 35 * M))).toEqual([[], ['E']])
    // nothing new: nothing to hold
    expect(codes(planLoad(meta, ['A'], ['A', 'B', 'C', 'D', 'E']))).toEqual([[], []])
    // unsharded data never ask
    expect(codes(planLoad({}, ['A'], []))).toEqual([[''], []])
    expect(codes(planLoad({}, [], ['']))).toEqual([[], []])
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
