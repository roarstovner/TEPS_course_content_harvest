// Character classes and case folding shared by the parser and the matcher.

// WORD[c] = 1 for a letter, digit or combining mark (UTF-16 code unit;
// surrogates count as non-word). Built once; a table lookup is much faster
// than a Unicode regex per character.
export const WORD = new Uint8Array(65536)
const SPACE = new Uint8Array(65536)
{
  const w = /[\p{L}\p{N}\p{M}]/u
  const s = /\s/u
  for (let c = 0; c < 65536; c++) {
    if (c >= 0xd800 && c <= 0xdfff) continue
    const ch = String.fromCharCode(c)
    if (w.test(ch)) WORD[c] = 1
    if (s.test(ch)) SPACE[c] = 1
  }
}

export const isWord = (s, i) => i >= 0 && i < s.length && WORD[s.charCodeAt(i)] === 1
export const isSpace = (s, i) => i >= 0 && i < s.length && SPACE[s.charCodeAt(i)] === 1

const MARKS = /\p{M}/gu

/**
 * Lower-case `s` without changing its length, so that an offset into the
 * folded string is the same offset into `s`. `toLowerCase()` changes the
 * length only for a few characters (U+0130 'İ' becomes 'i' + combining dot);
 * those are folded one by one, dropping the combining marks the lower-casing
 * added, or kept as they are if that does not restore the length. Final
 * sigma is folded to σ, so that it does not depend on the context.
 */
export function fold(s) {
  let l = s.toLowerCase()
  if (l.length !== s.length) {
    let out = ''
    for (const ch of s) {
      let c = ch.toLowerCase()
      if (c.length !== ch.length) c = c.normalize('NFD').replace(MARKS, '')
      if (c.length !== ch.length) c = ch
      out += c
    }
    l = out
  }
  if (l.indexOf('ς') !== -1) l = l.replaceAll('ς', 'σ')
  return l
}

/**
 * The word a match [start, end) falls in, from the folded text `lc`: the match
 * extended over letters and digits on both sides, across inner hyphens
 * (ikt-basert), and over a hyphen directly before whitespace or the end of
 * the text (fremmedspråks- og ...). Whitespace inside (a phrase) is collapsed
 * to one space.
 */
export function formAt(lc, start, end) {
  let i = start
  const n = lc.length
  while (i > 0) {
    const c = lc.charCodeAt(i - 1)
    if (WORD[c] === 1) i--
    else if (c === 45 && isWord(lc, i - 2) && isWord(lc, i)) i--
    else break
  }
  let j = end
  while (j < n) {
    const c = lc.charCodeAt(j)
    if (WORD[c] === 1) j++
    else if (c === 45 && isWord(lc, j - 1)) {
      if (isWord(lc, j + 1)) j++
      else {
        if (j + 1 >= n || isSpace(lc, j + 1)) j++
        break
      }
    } else break
  }
  const f = lc.slice(i, j)
  return /\s/.test(f) ? f.replace(/\s+/g, ' ') : f
}

// Does a regular expression with the flags i and u treat the character c as
// its lower case l? It compares characters by simple case folding, which
// equals lower-casing for nearly all characters.
const alike = new Map()
function foldsAlike(c, l) {
  if (c < 128) return true
  const k = c * 0x110000 + l
  let v = alike.get(k)
  if (v === undefined) alike.set(k, (v = new RegExp(`^\\u{${c.toString(16)}}$`, 'iu').test(String.fromCodePoint(l))))
  return v
}

function lowerBound(a, x) {
  let lo = 0
  let hi = a.length
  while (lo < hi) {
    const mid = (lo + hi) >>> 1
    if (a[mid] < x) lo = mid + 1
    else hi = mid
  }
  return lo
}

/**
 * A text held once: its folded copy `lc` (see `fold`) plus the offsets where
 * the original differs (`up`, null when nowhere) and the original characters
 * there (`uc`); about 3% of the plans' characters. The original, or a piece of
 * it, is rebuilt on demand (`text`, `slice`). `regexOnLc`: a case-insensitive
 * regular expression matches `lc` exactly as it matches the original (true
 * unless a character folds otherwise than it lower-cases).
 */
export class FoldedText {
  constructor(text) {
    const lc = fold(text)
    this.lc = lc
    this.up = null
    this.uc = ''
    this.regexOnLc = true
    if (lc === text) return
    const pos = []
    const chars = []
    for (let i = 0; i < lc.length; i++) {
      if (text.charCodeAt(i) === lc.charCodeAt(i)) continue
      pos.push(i)
      chars.push(text[i])
      const c = text.codePointAt(i)
      if (this.regexOnLc && !(c >= 0xdc00 && c <= 0xdfff)) this.regexOnLc = foldsAlike(c, lc.codePointAt(i))
    }
    this.up = lc.length < 65536 ? Uint16Array.from(pos) : Uint32Array.from(pos)
    this.uc = chars.join('')
  }
  get text() {
    return this.slice()
  }
  /** The original text from `s` to `e` (offsets as in `String.slice`, >= 0). */
  slice(s = 0, e = this.lc.length) {
    const { lc, up, uc } = this
    e = Math.min(e, lc.length)
    let i = up ? lowerBound(up, s) : 0
    if (!up || i === up.length || up[i] >= e) return lc.slice(s, e)
    const parts = []
    let p = s
    while (i < up.length && up[i] < e) {
      let j = i
      while (j + 1 < up.length && up[j + 1] === up[j] + 1 && up[j + 1] < e) j++
      parts.push(lc.slice(p, up[i]), uc.slice(i, j + 1))
      p = up[j] + 1
      i = j + 1
    }
    parts.push(lc.slice(p, e))
    return parts.join('')
  }
  /** Does the original differ from `lc` at offset i (an upper-case letter)? */
  differs(i) {
    const k = this.up ? lowerBound(this.up, i) : 0
    return !!this.up && k < this.up.length && this.up[k] === i
  }
}
