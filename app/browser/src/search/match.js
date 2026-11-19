// Term matchers: compile the terms of a parsed query into scanners over a
// text and its folded copy. Plain terms use indexOf on the folded text;
// regular expressions are built only where a term needs one.

import { fold, isSpace, isWord, WORD } from './text.js'
import { parseQuery } from './parse.js'

const escapeRe = (s) => s.replace(/[.*+?^${}()|[\]\\/-]/g, '\\$&')

// Each scanner calls hit(start, end) for every match, non-overlapping and in
// text order, and stops when hit returns true.

function plainScanner(value) {
  const lit = fold(value)
  const len = lit.length
  return (text, lc, hit) => {
    for (let p = lc.indexOf(lit); p !== -1; p = lc.indexOf(lit, p + len)) if (hit(p, p + len)) return
  }
}

// A wildcard term matches a whole run of letters and digits. The longest
// literal segment is found with indexOf; the run around it is then checked.
function wildcardScanner(value) {
  const pat = fold(value)
  const segs = pat.split('*')
  const anchor = segs.reduce((a, b) => (b.length > a.length ? b : a))
  const alen = anchor.length
  const starts = !pat.startsWith('*')
  const ends = !pat.endsWith('*')
  const nonEmpty = segs.filter(Boolean)
  let test = null // null: the position of the anchor decides
  if (nonEmpty.length > 1) {
    const inner = segs.map(escapeRe).join('[\\p{L}\\p{N}\\p{M}]*')
    test = new RegExp(`^${starts ? '' : '[\\p{L}\\p{N}\\p{M}]*'}${inner}${ends ? '' : '[\\p{L}\\p{N}\\p{M}]*'}$`, 'u')
  }
  return (text, lc, hit) => {
    const n = lc.length
    for (let p = lc.indexOf(anchor); p !== -1; ) {
      let rs = p
      while (rs > 0 && WORD[lc.charCodeAt(rs - 1)] === 1) rs--
      let re = p + alen
      while (re < n && WORD[lc.charCodeAt(re)] === 1) re++
      const ok = test ? test.test(lc.slice(rs, re)) : (!starts || rs === p) && (!ends || re === p + alen)
      if (ok && hit(rs, re)) return
      p = lc.indexOf(anchor, re)
    }
  }
}

// A quoted word or phrase: whole words at both ends, any whitespace between.
function quotedScanner(value) {
  const words = fold(value).split(/\s+/).filter(Boolean)
  const first = words[0]
  const last = words[words.length - 1]
  const leftB = WORD[first.charCodeAt(0)] === 1
  const rightB = WORD[last.charCodeAt(last.length - 1)] === 1
  return (text, lc, hit) => {
    for (let p = lc.indexOf(first); p !== -1; ) {
      let k = -1
      if (!leftB || !isWord(lc, p - 1)) {
        k = p + first.length
        for (let w = 1; w < words.length && k !== -1; w++) {
          const s = k
          while (isSpace(lc, k)) k++
          if (k === s || !lc.startsWith(words[w], k)) k = -1
          else k += words[w].length
        }
        if (k !== -1 && rightB && isWord(lc, k)) k = -1
      }
      if (k !== -1) {
        if (hit(p, k)) return
        p = lc.indexOf(first, k)
      } else p = lc.indexOf(first, p + 1)
    }
  }
}

function regexScanner(value) {
  const re = new RegExp(value, 'gimu')
  return (text, lc, hit) => {
    re.lastIndex = 0
    let m
    while ((m = re.exec(text)) !== null) {
      const s = m.index
      const e = s + m[0].length
      if (e === s) {
        re.lastIndex = s + 1
        continue
      }
      if (hit(s, e)) return
    }
  }
}

const SCANNERS = { plain: plainScanner, wildcard: wildcardScanner, quoted: quotedScanner, regex: regexScanner }

/**
 * Compile a query (a string or the result of `parseQuery`) into a matcher.
 * @returns {{ast, terms, error}} like `parseQuery`, with each term given
 *   `scan(text, lc, hit)`: calls `hit(start, end)` for each match in `text`
 *   (`lc` = `fold(text)`), stopping when `hit` returns true.
 */
export function compileQuery(query) {
  const parsed = typeof query === 'string' ? parseQuery(query) : query
  if (parsed.error || !parsed.ast) return { ...parsed, terms: [] }
  const terms = parsed.terms.map((t) => ({ ...t, scan: SCANNERS[t.kind](t.value) }))
  // the AST refers to the compiled terms by id
  const relink = (node) =>
    node.type === 'term'
      ? terms[node.id]
      : node.type === 'not'
        ? { type: 'not', child: relink(node.child) }
        : { type: node.type, children: node.children.map(relink) }
  return { ast: relink(parsed.ast), terms, error: null }
}
