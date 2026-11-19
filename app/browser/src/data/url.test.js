import { describe, expect, it } from 'vitest'
import { decodeState, defaultState, encodeState } from './url.js'

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
      course: 'uio|EDU:1000',
      version: 'abcdef012345',
      tab: 'sections',
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
  })

  it('falls back to defaults on invalid values', () => {
    const s = decodeState('scope=nonsense&n=-3&x=%7Bbroken&from=20x&course=nocolon')
    expect(s).toEqual(defaultState())
  })
})
