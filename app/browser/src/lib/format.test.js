import { expect, it } from 'vitest'
import { pct, segments } from './format.js'

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
