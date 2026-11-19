import { expect, it } from 'vitest'
import { pct, segments, segmentsIn } from './format.js'

it('splits a text into plain and marked pieces', () => {
  const marks = [
    { start: 0, end: 2, terms: [1] },
    { start: 4, end: 5, terms: [0, 2] },
  ]
  expect(segments('abcdef', marks)).toEqual([
    { text: 'ab', term: 1 },
    { text: 'cd', term: -1 },
    { text: 'e', term: 0 },
    { text: 'f', term: -1 },
  ])
  expect(segments('abc', [])).toEqual([{ text: 'abc', term: -1 }])
})

it('formats shares', () => {
  expect(pct(1, 3)).toBe('33.3%')
  expect(pct(1, 0)).toBe('–')
})

it('clips marks to a range of the text', () => {
  const marks = [
    { start: 1, end: 4, terms: [0] },
    { start: 6, end: 7, terms: [1] },
  ]
  expect(segmentsIn('abcdefgh', 2, 7, marks)).toEqual([
    { text: 'cd', term: 0 },
    { text: 'ef', term: -1 },
    { text: 'g', term: 1 },
  ])
})
