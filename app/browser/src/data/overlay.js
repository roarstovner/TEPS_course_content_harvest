// Which section each line of a plan went to. Sections are cut from the plan
// text and cleaned, so a plan line is matched to a section by its normalized
// text (case folded, whitespace collapsed), not by position.

/** A line for comparison: lower case, runs of whitespace as one space, trimmed. */
export const normLine = (s) => s.toLowerCase().replace(/\s+/g, ' ').trim()

/**
 * The section of each line of `text`.
 * @param {string} text the plan
 * @param {[string, string][]} sections [code, text] in the order of preference
 * @returns {{text: string, start: number, section: string|null}[]} one per
 *   line of `text` (`start` its offset)
 *   (`section` null for blank lines and lines in no section). A line found in several
 *   sections goes to the section of the nearest line above it that is one of
 *   them, else to the first.
 */
export function lineSections(text, sections) {
  const where = new Map() // normalized line -> codes
  for (const [code, t] of sections)
    for (const l of t.split('\n')) {
      const n = normLine(l)
      if (!n) continue
      const codes = where.get(n)
      if (!codes) where.set(n, [code])
      else if (!codes.includes(code)) codes.push(code)
    }
  let prev = null
  let start = 0
  return text.split('\n').map((l) => {
    const codes = where.get(normLine(l))
    const section = codes ? (codes.includes(prev) ? prev : codes[0]) : null
    if (section) prev = section
    start += l.length + 1
    return { text: l, start: start - l.length - 1, section }
  })
}

/**
 * How well the lines match: plan lines (non-blank) with a section, and
 * section lines (non-blank) found among the plan's lines.
 * @returns {{planLines, planInSection, sectionLines, sectionInPlan}}
 */
export function overlayCoverage(text, sections) {
  const lines = lineSections(text, sections).filter((l) => normLine(l.text))
  const inPlan = new Set(lines.map((l) => normLine(l.text)))
  let sectionLines = 0
  let sectionInPlan = 0
  for (const [, t] of sections)
    for (const l of t.split('\n')) {
      const n = normLine(l)
      if (!n) continue
      sectionLines++
      if (inPlan.has(n)) sectionInPlan++
    }
  return { planLines: lines.length, planInSection: lines.filter((l) => l.section).length, sectionLines, sectionInPlan }
}

/**
 * Consecutive lines of `lineSections` as blocks: `kind` a section code,
 * 'none' (in no section) or 'blank' (blank lines only). Blank lines between
 * two lines of the same kind join them.
 * @returns {{kind: string, start: number, end: number}[]} [start, end) in the
 *   plan text, without the line break that ends the block
 */
export function lineBlocks(lines) {
  const kinds = lines.map((l) => (normLine(l.text) ? (l.section ?? 'none') : 'blank'))
  for (let i = 0; i < kinds.length; i++) {
    if (kinds[i] !== 'blank') continue
    let j = i
    while (kinds[j] === 'blank') j++
    const k = i > 0 && kinds[i - 1] === kinds[j] ? kinds[j] : 'blank'
    kinds.fill(k, i, j)
    i = j - 1
  }
  const out = []
  lines.forEach((l, i) => {
    const end = l.start + l.text.length
    if (out.at(-1)?.kind === kinds[i]) out.at(-1).end = end
    else out.push({ kind: kinds[i], start: l.start, end })
  })
  return out
}
