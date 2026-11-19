// Query parser: string -> AST. See the README, "Query Syntax".
//
// Grammar (OR binds tighter than the implicit AND, NOT tighter than OR):
//   query   = and
//   and     = or { ["AND"] or }
//   or      = unary { "OR" unary }
//   unary   = ("-" | "NOT") unary | [prefix ":"] primary
//   primary = "(" and ")" | term | '"' words '"' | "/" regex "/"
//
// AST nodes: { type: 'and' | 'or', children }, { type: 'not', child },
// { type: 'term', id, kind, value, section, key, start, end }, where kind is
// 'plain', 'wildcard', 'quoted' or 'regex' and section a section code or null.

import { fold } from './text.js'

/** Section codes, their prefix in queries and the accepted aliases. */
export const SECTIONS = [
  ['course_content', 'content', 'innhold'],
  ['learning_outcomes', 'outcomes', 'læringsutbytte'],
  ['prerequisites', 'prerequisites', 'forkunnskaper'],
  ['teaching_methods', 'teaching', 'undervisning'],
  ['coursework_requirements', 'coursework', 'arbeidskrav'],
  ['assessment', 'assessment', 'vurdering'],
  ['reading_list', 'reading', 'pensum'],
  ['practice', 'practice', 'praksis'],
]
const PREFIX = new Map()
const SHORT = new Map()
for (const [code, ...names] of SECTIONS) {
  SHORT.set(code, names[0])
  for (const n of [code, ...names]) PREFIX.set(n, code)
}

class QueryError extends Error {
  constructor(message, start, end) {
    super(message)
    this.start = start
    this.end = end ?? start + 1
  }
}

function tokenize(q) {
  const toks = []
  let i = 0
  const n = q.length
  const ws = (c) => /\s/.test(c)
  while (i < n) {
    const c = q[i]
    if (ws(c)) { i++; continue }
    const start = i
    if (c === '(' || c === ')') {
      toks.push({ t: c, start, end: ++i })
    } else if (c === '-' && i + 1 < n && !ws(q[i + 1])) {
      toks.push({ t: 'NOT', start, end: ++i })
    } else if (c === '-') {
      throw new QueryError('"-" must be followed directly by the term to exclude, as in -praksis', start)
    } else if (c === '"') {
      const close = q.indexOf('"', i + 1)
      if (close === -1) throw new QueryError('Unclosed quote: add the closing "', start, n)
      const value = q.slice(i + 1, close).trim()
      if (!value) throw new QueryError('Empty quotes', start, close + 1)
      i = close + 1
      toks.push({ t: 'TERM', kind: 'quoted', value, start, end: i })
    } else if (c === '/') {
      let j = i + 1
      let inClass = false
      for (; j < n; j++) {
        const d = q[j]
        if (d === '\\') j++
        else if (d === '[') inClass = true
        else if (d === ']') inClass = false
        else if (d === '/' && !inClass) break
      }
      if (j >= n) throw new QueryError('Unclosed regular expression: add the closing /', start, n)
      const value = q.slice(i + 1, j)
      if (!value) throw new QueryError('Empty regular expression', start, j + 1)
      try {
        new RegExp(value, 'iu')
      } catch (e) {
        throw new QueryError(`Invalid regular expression: ${e.message}`, start, j + 1)
      }
      i = j + 1
      toks.push({ t: 'TERM', kind: 'regex', value, start, end: i })
    } else {
      let j = i
      while (j < n && !ws(q[j]) && q[j] !== '(' && q[j] !== ')') j++
      const word = q.slice(i, j)
      const m = /^([\p{L}_]+):/u.exec(word)
      const code = m && PREFIX.get(m[1].toLowerCase())
      if (code) {
        i += m[0].length
        if (i >= n || ws(q[i])) throw new QueryError(`"${m[0]}" must be followed directly by a term, as in ${m[0]}dybdelæring`, start, i)
        toks.push({ t: 'PREFIX', code, start, end: i })
        continue
      }
      i = j
      if (word === 'OR' || word === 'AND' || word === 'NOT') toks.push({ t: word, start, end: i })
      else if (word.includes('*')) {
        if (!word.replaceAll('*', '')) throw new QueryError('A wildcard term needs letters besides *', start, i)
        toks.push({ t: 'TERM', kind: 'wildcard', value: word, start, end: i })
      } else toks.push({ t: 'TERM', kind: 'plain', value: word, start, end: i })
    }
  }
  return toks
}

function parseTokens(toks, qlen) {
  let k = 0
  const peek = () => toks[k]
  const atEnd = () => k >= toks.length
  const endPos = () => (atEnd() ? qlen : toks[k].start)
  const operand = (op) => {
    const tk = peek()
    if (!tk || tk.t === ')' || tk.t === 'OR' || tk.t === 'AND')
      throw new QueryError(`${op.t === 'PREFIX' ? 'A section prefix' : op.t} must be followed by a term`, op.start, op.end)
  }

  function parseAnd() {
    const children = []
    while (!atEnd() && peek().t !== ')') {
      if (peek().t === 'AND') {
        const op = toks[k++]
        if (!children.length) throw new QueryError('AND needs a term before it', op.start, op.end)
        operand(op)
        continue
      }
      children.push(parseOr())
    }
    return children.length === 1 ? children[0] : { type: 'and', children }
  }
  function parseOr() {
    if (peek().t === 'OR') throw new QueryError('OR needs a term before it', peek().start, peek().end)
    const children = [parseUnary()]
    while (!atEnd() && peek().t === 'OR') {
      const op = toks[k++]
      operand(op)
      children.push(parseUnary())
    }
    return children.length === 1 ? children[0] : { type: 'or', children }
  }
  function parseUnary() {
    const tk = peek()
    if (tk.t === 'NOT') {
      k++
      operand(tk)
      return { type: 'not', child: parseUnary() }
    }
    if (tk.t === 'PREFIX') {
      k++
      operand(tk)
      if (peek().t === 'NOT' || peek().t === 'PREFIX')
        throw new QueryError('Put the exclusion before the section prefix, as in -outcomes:praksis', peek().start, peek().end)
      const node = parsePrimary()
      setSection(node, tk.code)
      return node
    }
    return parsePrimary()
  }
  function parsePrimary() {
    const tk = toks[k++]
    if (tk.t === 'TERM') return { type: 'term', kind: tk.kind, value: tk.value, section: null, start: tk.start, end: tk.end }
    if (tk.t === '(') {
      if (!atEnd() && peek().t === ')') throw new QueryError('Empty parentheses', tk.start, peek().end)
      const inner = parseAnd()
      if (atEnd()) throw new QueryError('Unclosed parenthesis: add the closing )', tk.start, endPos())
      k++
      return inner
    }
    if (tk.t === ')') throw new QueryError('Closing parenthesis without an opening one', tk.start, tk.end)
    throw new QueryError(`Unexpected ${tk.t}`, tk.start, tk.end)
  }

  const ast = parseAnd()
  if (!atEnd()) throw new QueryError('Closing parenthesis without an opening one', peek().start, peek().end)
  return ast
}

// A prefix on a group applies to its terms; a prefix inside the group wins.
function setSection(node, code) {
  if (node.type === 'term') node.section ??= code
  else if (node.type === 'not') setSection(node.child, code)
  else node.children.forEach((c) => setSection(c, code))
}

function termKey(t) {
  const v =
    t.kind === 'regex' ? `/${t.value}/` : t.kind === 'quoted' ? `"${fold(t.value).replace(/\s+/g, ' ')}"` : fold(t.value)
  return t.section ? `${SHORT.get(t.section)}:${v}` : v
}

/**
 * Parse a query.
 * @param {string} q
 * @returns {{ast: object|null, terms: object[], error: null | {message: string, start: number, end: number}}}
 *   `terms` are the term nodes in query order (`terms[i].id === i`), each
 *   with `negated` (under a NOT) and `key` (canonical text, e.g.
 *   `outcomes:utforsk*`, the key of its excluded forms). An empty query gives
 *   `ast: null` and no error. Errors never throw; `start`/`end` locate the
 *   problem in `q`.
 */
export function parseQuery(q) {
  q = q ?? ''
  try {
    const toks = tokenize(q)
    if (!toks.length) return { ast: null, terms: [], error: null }
    const ast = parseTokens(toks, q.length)
    const terms = []
    const walk = (node, negated) => {
      if (node.type === 'term') {
        node.id = terms.length
        node.negated = negated
        node.key = termKey(node)
        terms.push(node)
      } else if (node.type === 'not') walk(node.child, true)
      else node.children.forEach((c) => walk(c, negated))
    }
    walk(ast, false)
    return { ast, terms, error: null }
  } catch (e) {
    if (!(e instanceof QueryError)) throw e
    return { ast: null, terms: [], error: { message: e.message, start: e.start, end: e.end } }
  }
}
