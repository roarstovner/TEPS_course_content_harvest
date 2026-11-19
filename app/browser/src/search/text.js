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
