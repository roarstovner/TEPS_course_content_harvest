import { expect, it } from 'vitest'
import { lineBlocks, lineSections, normLine, overlayCoverage } from './overlay.js'

const plan = 'Emne X\nLæringsutbytte\nKunnskap:\nStudenten  kan lese.\n\nVurdering\nKunnskap:\nEksamen'
const sections = [
  ['learning_outcomes', 'kunnskap:\nStudenten kan lese.'],
  ['assessment', 'Kunnskap:\n eksamen \nlinje som ble renset bort'],
]

it('normalizes lines', () => expect(normLine('  A  b\tC ')).toBe('a b c'))

it('maps plan lines to sections by their normalized text', () => {
  const ls = lineSections(plan, sections)
  expect(ls.map((l) => l.section)).toEqual([
    null,
    null,
    'learning_outcomes',
    'learning_outcomes',
    null,
    null,
    'learning_outcomes', // in both: the nearest section above wins
    'assessment',
  ])
  expect(ls.map((l) => plan.slice(l.start, l.start + l.text.length))).toEqual(plan.split('\n'))
})

it('a line in several sections goes to the first without context', () => {
  expect(lineSections('Kunnskap:', sections)[0].section).toBe('learning_outcomes')
})

it('measures coverage both ways', () => {
  expect(overlayCoverage(plan, sections)).toEqual({ planLines: 7, planInSection: 4, sectionLines: 5, sectionInPlan: 4 })
})

it('groups lines into blocks; blank lines inside a section join it', () => {
  const text = 'A\n\nB\n\nC\nD'
  const ls = lineSections(text, [['x', 'a\nb'], ['y', 'd']])
  expect(lineBlocks(ls).map((b) => [b.kind, text.slice(b.start, b.end)])).toEqual([
    ['x', 'A\n\nB'],
    ['blank', ''],
    ['none', 'C'],
    ['y', 'D'],
  ])
})
