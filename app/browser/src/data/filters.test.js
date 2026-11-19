import { describe, expect, it } from 'vitest'
import { creditBin, filterValues, valueLabel } from './filters.js'

describe('course filters', () => {
  it('bins credits and names the values', () => {
    expect([0, 2.5, 7.5, 9, 10, 15, 22.5, 30, 60, null].map(creditBin)).toEqual(['0', 'lt7.5', '7.5', '7.5', '10', '15', '20', '30', 'gt30', ''])
    expect(valueLabel('lvl', 'HN')).toBe('Higher degree')
    expect(valueLabel('lang', 'SME')).toBe('Northern Sami')
    expect(valueLabel('lang', 'XYZ')).toBe('XYZ')
    expect(valueLabel('subj', '')).toBe('(not given)')
  })
  it('orders credits by range and other values by courses', () => {
    const cs = [15, 30, 0, 15, 5].map((credits, i) => ({ credits, language_code: i ? 'NOR' : 'ENG' }))
    expect(filterValues(cs, 'cr')).toEqual(['0', 'lt7.5', '15', '30'])
    expect(filterValues(cs, 'lang')).toEqual(['NOR', 'ENG'])
  })
})
