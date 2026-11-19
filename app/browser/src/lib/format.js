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
export function segments(text, marks) {
  const out = []
  let pos = 0
  for (const m of marks) {
    if (m.start > pos) out.push({ text: text.slice(pos, m.start), term: -1 })
    out.push({ text: text.slice(m.start, m.end), term: m.terms[0] })
    pos = m.end
  }
  if (pos < text.length) out.push({ text: text.slice(pos), term: -1 })
  return out
}
