import { expect, it } from 'vitest'
import * as jsdiff from 'diff'
import { diffStats, sides, wordDiff } from './diff.js'

const spell = (parts, ops) => parts.filter((p) => ops.includes(p.op)).map((p) => p.text).join('')

it('marks inserted and deleted words, with offsets into either text', async () => {
  const a = 'Emnet gir kunnskap om lesing.\nVurdering: eksamen\nPensum: liste'
  const b = 'Emnet gir god kunnskap om skriving.\nVurdering: eksamen\nPensum: liste\nNy linje'
  const parts = await wordDiff(a, b, { jsdiff })
  expect(spell(parts, ['eq', 'ins'])).toBe(b)
  expect(spell(parts, ['eq', 'del'])).toBe(a)
  for (const p of parts) expect((p.op === 'del' ? a : b).slice(p.start, p.start + p.text.length)).toBe(p.text)
  expect(parts.filter((p) => p.op === 'del').map((p) => p.text.trim())).toEqual(['lesing'])
  expect(diffStats(parts)).toEqual({ ins: 4, del: 1 })
})

it('compares identical and empty texts', async () => {
  expect(await wordDiff('a b', 'a b', { jsdiff })).toEqual([{ op: 'eq', text: 'a b', start: 0 }])
  expect(await wordDiff('', 'x', { jsdiff })).toEqual([{ op: 'ins', text: 'x', start: 0 }])
})

it('splits a comparison into two sides with offsets into each text', async () => {
  const a = 'Emnet gir kunnskap om lesing.\nVurdering: eksamen'
  const b = 'Emnet gir god kunnskap om skriving.\nVurdering: hjemmeeksamen'
  const { a: left, b: right } = sides(await wordDiff(a, b, { jsdiff }))
  expect(left.map((p) => p.text).join('')).toBe(a)
  expect(right.map((p) => p.text).join('')).toBe(b)
  expect(left.some((p) => p.op === 'ins') || right.some((p) => p.op === 'del')).toBe(false)
  for (const p of left) expect(a.slice(p.start, p.start + p.text.length)).toBe(p.text)
  for (const p of right) expect(b.slice(p.start, p.start + p.text.length)).toBe(p.text)
})
