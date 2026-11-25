// Word-level comparison of two plan versions. jsdiff is loaded on first use,
// so that it stays out of the main bundle.

/**
 * Changes as parts with their offsets: `start` is in `b` for 'eq' and 'ins'
 * parts, in `a` for 'del' parts. Whitespace runs are tokens of their own, so
 * the 'eq' + 'ins' parts spell `b` and the 'eq' + 'del' parts spell `a`.
 * @param {{value: string, added?: boolean, removed?: boolean}[]} changes jsdiff output
 * @returns {{op: 'eq'|'ins'|'del', text: string, start: number}[]}
 */
export function toParts(changes) {
  let ia = 0
  let ib = 0
  return changes.map(({ value: text, added, removed }) => {
    if (removed) return { op: 'del', text, start: (ia += text.length) - text.length }
    if (added) return { op: 'ins', text, start: (ib += text.length) - text.length }
    ia += text.length
    return { op: 'eq', text, start: (ib += text.length) - text.length }
  })
}

/**
 * Compare `a` (older) with `b` (newer) word by word: lines first, then the
 * words of each changed run of lines (fast where few lines changed; a run
 * that takes longer than `timeout` ms stays a whole deletion and insertion).
 * @param {object} [jsdiff] the jsdiff module; loaded when not given
 * @returns {Promise<{op, text, start}[]>}
 */
export async function wordDiff(a, b, { timeout = 300, jsdiff } = {}) {
  const { diffLines, diffWordsWithSpace } = jsdiff ?? (await import('diff'))
  const out = []
  let del = ''
  let ins = ''
  const flush = () => {
    if (del && ins) out.push(...(diffWordsWithSpace(del, ins, { timeout }) ?? [{ value: del, removed: true }, { value: ins, added: true }]))
    else if (del || ins) out.push({ value: del || ins, removed: !!del, added: !!ins })
    del = ins = ''
  }
  for (const c of diffLines(a, b)) {
    if (c.removed) del += c.value
    else if (c.added) ins += c.value
    else flush(), out.push(c)
  }
  flush()
  return toParts(out)
}

/** Words inserted and deleted (whitespace parts not counted). */
export const diffStats = (parts) => {
  const words = (s) => s.split(/\s+/).filter(Boolean).length
  const n = { ins: 0, del: 0 }
  for (const p of parts) if (p.op !== 'eq') n[p.op] += words(p.text)
  return n
}

/** The parts of each side, for a side-by-side view: `a` the old text with
 *  its deletions, `b` the new with its insertions; `start` is the offset in
 *  that side's text. */
export function sides(parts) {
  const side = (skip) => {
    let pos = 0
    return parts.filter((p) => p.op !== skip).map((p) => ({ ...p, start: (pos += p.text.length) - p.text.length }))
  }
  return { a: side('ins'), b: side('del') }
}
