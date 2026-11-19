import { describe, expect, it } from 'vitest'
import { parseQuery } from './parse.js'

// compact rendering of an AST for assertions
const show = (n) =>
  n.type === 'term'
    ? (n.section ? `${n.section}:` : '') + (n.kind === 'quoted' ? `"${n.value}"` : n.kind === 'regex' ? `/${n.value}/` : n.value)
    : n.type === 'not'
      ? `NOT(${show(n.child)})`
      : `${n.type.toUpperCase()}(${n.children.map(show).join(' ')})`
const ast = (q) => {
  const r = parseQuery(q)
  expect(r.error).toBeNull()
  return show(r.ast)
}
const err = (q) => {
  const r = parseQuery(q)
  expect(r.ast).toBeNull()
  expect(r.error).not.toBeNull()
  return r.error
}

describe('parseQuery', () => {
  it('treats whitespace as AND', () => {
    expect(ast('fremmedspråk* morsmål*')).toBe('AND(fremmedspråk* morsmål*)')
    expect(ast('a AND b')).toBe('AND(a b)')
  })
  it('binds OR tighter than AND', () => {
    expect(ast('a b OR c')).toBe('AND(a OR(b c))')
    expect(ast('a OR b c OR d')).toBe('AND(OR(a b) OR(c d))')
    expect(ast('a OR b OR c')).toBe('OR(a b c)')
  })
  it('groups with parentheses', () => {
    expect(ast('(lesing OR skriving) vurdering')).toBe('AND(OR(lesing skriving) vurdering)')
    expect(ast('(a b) OR c')).toBe('OR(AND(a b) c)')
  })
  it('parses - and NOT, tighter than OR', () => {
    expect(ast('-praksis')).toBe('NOT(praksis)')
    expect(ast('a NOT praksis')).toBe('AND(a NOT(praksis))')
    expect(ast('-a OR b')).toBe('OR(NOT(a) b)')
    expect(ast('-(a OR b)')).toBe('NOT(OR(a b))')
  })
  it('keeps lower-case or/not and inner hyphens as terms', () => {
    expect(ast('lese- or 1-7')).toBe('AND(lese- or 1-7)')
  })
  it('parses quotes, phrases, wildcards and regexes', () => {
    expect(ast('"ikt" "folkehelse og livsmestring"')).toBe('AND("ikt" "folkehelse og livsmestring")')
    const r = parseQuery('ut*ing /utforsk(e|ende)/ /a\\/b[/]/')
    expect(r.terms.map((t) => t.kind)).toEqual(['wildcard', 'regex', 'regex'])
    expect(r.terms[2].value).toBe('a\\/b[/]')
  })
  it('parses section prefixes and aliases, also on groups', () => {
    expect(ast('outcomes:dybdelæring')).toBe('learning_outcomes:dybdelæring')
    expect(ast('læringsutbytte:"ikt" pensum:/x/')).toBe('AND(learning_outcomes:"ikt" reading_list:/x/)')
    expect(ast('praksis:(a OR content:b)')).toBe('OR(practice:a course_content:b)')
    expect(ast('-outcomes:praksis')).toBe('NOT(learning_outcomes:praksis)')
    expect(ast('foo:bar')).toBe('foo:bar') // not a section: a plain term
  })
  it('numbers terms, marks negated ones and gives canonical keys', () => {
    const r = parseQuery('Utforsk* -outcomes:Praksis "IKT  og"')
    expect(r.terms.map((t) => [t.id, t.negated, t.key])).toEqual([
      [0, false, 'utforsk*'],
      [1, true, 'outcomes:praksis'],
      [2, false, '"ikt og"'],
    ])
  })
  it('accepts an empty query', () => {
    expect(parseQuery('  ')).toEqual({ ast: null, terms: [], error: null })
  })
  it('reports errors with a position instead of throwing', () => {
    expect(err('a "folkehelse og')).toMatchObject({ message: expect.stringMatching(/quote/i), start: 2 })
    expect(err('(a OR b')).toMatchObject({ message: expect.stringMatching(/parenthesis/i), start: 0 })
    expect(err('a OR b)')).toMatchObject({ message: expect.stringMatching(/parenthesis/i), start: 6 })
    expect(err('a ()')).toMatchObject({ message: expect.stringMatching(/empty/i), start: 2 })
    expect(err('OR a').message).toMatch(/OR needs a term before/)
    expect(err('a OR').message).toMatch(/OR must be followed/)
    expect(err('a NOT').message).toMatch(/NOT must be followed/)
    expect(err('a - b').start).toBe(2)
    expect(err('/(a/')).toMatchObject({ message: expect.stringMatching(/Invalid regular expression/), start: 0 })
    expect(err('/abc').message).toMatch(/Unclosed regular expression/)
    expect(err('outcomes: x').message).toMatch(/followed directly/)
    expect(err('""').message).toMatch(/Empty quotes/)
    expect(err('**').message).toMatch(/wildcard/)
  })
})
