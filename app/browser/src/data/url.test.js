import { describe, expect, it } from 'vitest'
import { applyExample, decodeState, defaultState, encodeState } from './url.js'

describe('URL state', () => {
  it('writes nothing for the defaults', () => {
    expect(encodeState(defaultState())).toBe('')
    expect(decodeState('')).toEqual(defaultState())
  })

  it('round-trips every field', () => {
    const s = {
      ...defaultState(),
      q: 'fremmedspråk* "ikt" outcomes:(a OR b)',
      scope: 'words',
      n: 5,
      excl: { 'fremmedspråk*': ['fremmedspråksenteret', 'a b'] },
      inst: ['uio', 'ntnu'],
      y0: 2018,
      y1: 2024,
      st: ['IMALU1-7'],
      subj: ['Norsk, nynorsk', 'Pedagogikk'],
      nus: ['22', '53'],
      lvl: ['LN', 'HN'],
      cr: ['lt7.5', '15'],
      lang: ['ENG'],
      sec: ['learning_outcomes', 'reading_list'],
      course: 'uio|EDU:1000',
      version: 'abcdef012345',
      tab: 'sections',
      view: 'trend',
      by: 'inst',
    }
    expect(decodeState(encodeState(s))).toEqual(s)
  })

  it('keeps the compared version on the versions tab only', () => {
    const s = { ...defaultState(), course: 'uio|A', tab: 'versions', cmp: 'abc' }
    expect(decodeState(encodeState(s))).toEqual(s)
    expect(encodeState({ ...s, tab: 'source' })).toBe('course=uio%3AA&tab=source')
  })

  it('drops N outside the words scope, empty exclusions and the version without a course', () => {
    const s = { ...defaultState(), n: 4, excl: { a: [] }, version: 'x' }
    expect(encodeState(s)).toBe('')
    expect(encodeState({ ...defaultState(), view: 'coverage', by: 'st' })).toBe('view=coverage')
    expect(encodeState({ ...defaultState(), sample: 20, seed: '42' })).toBe('')
  })

  it('keeps the sample size and seed in the screening view', () => {
    const s = { ...defaultState(), q: 'ikt', view: 'screen', sample: 20, seed: '123456' }
    expect(encodeState(s)).toBe('q=ikt&view=screen&sample=20&seed=123456')
    expect(decodeState(encodeState(s))).toEqual(s)
  })

  it('keeps what is compared in the compare view', () => {
    const pair = { ...defaultState(), view: 'compare', ca: 'uio|EDU:1000@abcdef012345', cb: 'ntnu|MA1@0123456789ab', csec: 'learning_outcomes', diff: false }
    expect(encodeState(pair)).toBe('view=compare&ca=uio%3AEDU%3A1000%40abcdef012345&cb=ntnu%3AMA1%400123456789ab&diff=0&csec=learning_outcomes')
    expect(decodeState(encodeState(pair))).toEqual(pair)
    const across = { ...defaultState(), view: 'compare', cm: 'across', cyear: 2024, st: ['GLU1-7'] }
    expect(decodeState(encodeState(across))).toEqual(across)
    // the plans of the pair are not written across institutions, nor anything outside the view
    expect(encodeState({ ...across, ca: 'uio|A@x' })).toBe(encodeState(across))
    expect(encodeState({ ...pair, view: 'list' })).toBe('')
    expect(decodeState('view=compare&ca=nocolon&cm=x&cyear=24').ca).toBeNull()
  })

  it('keeps the term list and heatmap settings in the term list view', () => {
    const s = { ...defaultState(), view: 'terms', tl: 'LK20 terms', trows: 'inst', tval: 'count', y0: 2015 }
    expect(encodeState(s)).toBe('from=2015&view=terms&tl=LK20+terms&trows=inst&tval=count')
    expect(decodeState(encodeState(s))).toEqual(s)
    expect(encodeState({ ...s, view: 'list' })).toBe('from=2015')
    expect(decodeState('view=terms&trows=x&tval=y')).toEqual({ ...defaultState(), view: 'terms' })
  })

  it('keeps the help view, and runs a guide example with the other filters kept', () => {
    const help = { ...defaultState(), view: 'help', q: 'samisk' }
    expect(encodeState(help)).toBe('q=samisk&view=help')
    expect(decodeState(encodeState(help))).toEqual(help)
    const s = { ...help, inst: ['uit'], sec: ['practice'], course: 'uit|EDU-1@x', scope: 'sentence' }
    applyExample(s, '?q=outcomes:samisk%20content:urfolk')
    expect(s).toMatchObject({ q: 'outcomes:samisk content:urfolk', scope: 'plan', sec: [], view: 'list', inst: ['uit'], course: 'uit|EDU-1@x' })
    applyExample(s, '?q=samisk&view=trend&by=inst&sec=learning_outcomes')
    expect(s).toMatchObject({ view: 'trend', by: 'inst', sec: ['learning_outcomes'] })
  })

  it('keeps the section searched, by its name, in every view', () => {
    const s = { ...defaultState(), q: 'samisk', in: 'learning_outcomes' }
    expect(encodeState(s)).toBe('q=samisk&in=outcomes')
    expect(decodeState(encodeState(s))).toEqual(s)
    expect(decodeState(encodeState({ ...s, view: 'trend' }))).toEqual({ ...s, view: 'trend' })
    expect(decodeState('in=l%C3%A6ringsutbytte').in).toBe('learning_outcomes')
    expect(decodeState('in=reading_list').in).toBe('reading_list')
    expect(decodeState('in=nonsense').in).toBeNull()
    expect(encodeState({ ...defaultState(), in: 'nonsense' })).toBe('')
    // a guide example sets it (or clears it) like the scope
    const t = { ...s, inst: ['uit'] }
    applyExample(t, '?q=samisk&in=reading')
    expect(t).toMatchObject({ q: 'samisk', in: 'reading_list', inst: ['uit'] })
    applyExample(t, '?q=samisk')
    expect(t.in).toBeNull()
  })

  it('keeps the section coverage in the coverage view', () => {
    const s = { ...defaultState(), view: 'coverage', cov: 'sections' }
    expect(encodeState(s)).toBe('view=coverage&cov=sections')
    expect(decodeState(encodeState(s))).toEqual(s)
    expect(encodeState({ ...s, view: 'list' })).toBe('')
    expect(decodeState('view=coverage&cov=x').cov).toBe('plans')
  })

  it('falls back to defaults on invalid values', () => {
    const s = decodeState('scope=nonsense&n=-3&x=%7Bbroken&from=20x&course=nocolon&view=chart&by=x&sample=0&seed=a%20b&in=x&cov=y')
    expect(s).toEqual(defaultState())
  })
})
