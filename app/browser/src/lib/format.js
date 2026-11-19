// Number formatting and text marking for the components.

export const num = (n) => n.toLocaleString('en')
export const pct = (a, b) => (b ? `${((100 * a) / b).toFixed(1)}%` : '–')
export const mb = (bytes) => `${(bytes / 1e6).toFixed(1)} MB`

/**
 * Split a text into plain and marked pieces.
 * @param {string} text
 * @param {{start: number, end: number, terms: number[]}[]} marks sorted, disjoint
 * @returns {{text: string, term: number}[]} term -1 for plain text
 */
export const segments = (text, marks) => segmentsIn(text, 0, text.length, marks)

/** `segments` of the range [start, end) of `text`; marks are clipped to it. */
export function segmentsIn(text, start, end, marks) {
  const out = []
  let pos = start
  for (const m of marks) {
    if (m.end <= start) continue
    if (m.start >= end) break
    const s = Math.max(m.start, start)
    const e = Math.min(m.end, end)
    if (s > pos) out.push({ text: text.slice(pos, s), term: -1 })
    out.push({ text: text.slice(s, e), term: m.terms[0] })
    pos = e
  }
  if (pos < end) out.push({ text: text.slice(pos, end), term: -1 })
  return out
}
