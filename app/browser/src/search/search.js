// Search over prepared plans: matching, word forms, keyword in context and
// highlighting. Pure functions without DOM, for use in a Web Worker.

import { fold, formAt, isSpace, WORD } from './text.js'

/**
 * Prepare plans for searching: keep a folded (lower-cased) copy of each text
 * whose offsets equal those of the original (see `fold`).
 * @param {{id: any, text: string, sections?: Object<string, string>}[]} plans
 * @returns {{id, text, lc, sections: Object<string, {text, lc}>}[]}
 */
export function prepareDocs(plans) {
  return plans.map((p) => {
    const text = p.text ?? ''
    const sections = {}
    for (const [code, t] of Object.entries(p.sections ?? {})) if (t) sections[code] = { text: t, lc: fold(t) }
    return { id: p.id, text, lc: fold(text), sections }
  })
}

// excludedForms: {termKey: forms} or Map -> array of Set by term id
function exclusions(terms, excludedForms) {
  if (!excludedForms) return terms.map(() => null)
  const get = (k) => (excludedForms instanceof Map ? excludedForms.get(k) : excludedForms[k])
  return terms.map((t) => {
    const f = get(t.key)
    const set = f ? new Set([...f].map(fold)) : null
    return set && set.size ? set : null
  })
}

// The texts a term is matched in: its section (prefix, or the search's
// section), else the plan text; in the section-based scopes an unprefixed
// positive term is matched in every section.
function textsOf(term, doc, section, perSection) {
  const code = term.section ?? section
  if (code) {
    const s = doc.sections[code]
    return s ? [[code, s.text, s.lc]] : []
  }
  if (perSection && !term.negated) return Object.entries(doc.sections).map(([k, s]) => [k, s.text, s.lc])
  return [['', doc.text, doc.lc]]
}

// Count the matches of a term in texts, without those inside excluded forms;
// `first`: stop at the first.
function count(term, texts, excl, first) {
  let n = 0
  for (const [, text, lc] of texts) {
    term.scan(text, lc, (s, e) => {
      if (excl && excl.has(formAt(lc, s, e))) return false
      n++
      return first
    })
    if (first && n) break
  }
  return n
}

function hitList(term, texts, excl) {
  const out = []
  for (const [key, text, lc] of texts)
    term.scan(text, lc, (s, e) => {
      if (!excl || !excl.has(formAt(lc, s, e))) out.push([key, s, e, text, lc])
      return false
    })
  return out
}

// Start of the unit (line or sentence) that position s falls in.
function lineStart(text, s) {
  return text.lastIndexOf('\n', s - 1) + 1
}
function sentenceStart(text, lc, s) {
  for (let i = s - 1; i >= 0; i--) {
    const c = text[i]
    if (c === '\n') return i + 1
    if ((c === '.' || c === '!' || c === '?') && isSpace(text, i + 1)) {
      let j = i + 1
      while (j < s && isSpace(text, j)) j++
      if (j <= s && (text[j] !== lc[j] || /\d/.test(text[j]))) return j
    }
  }
  return 0
}
const SCOPES = new Set(['plan', 'section', 'paragraph', 'sentence', 'words'])

/**
 * Find the plans that match.
 * @param {object[]} docs from `prepareDocs`
 * @param {object} matcher from `compileQuery`
 * @param {object} [opts]
 * @param {'plan'|'section'|'paragraph'|'sentence'|'words'} [opts.scope='plan']
 *   where all positive terms must occur together
 * @param {number} [opts.words=10] for scope 'words': the largest distance in
 *   words between the first and the last matched word
 * @param {string} [opts.section] search one section only (a section code)
 * @param {Object<string, Iterable<string>>|Map} [opts.excludedForms] by term key
 * @returns {{id, counts: number[]}[]} matching plans in the order of `docs`;
 *   `counts[i]` = the number of matches of term i in the plan (in the texts
 *   the term is matched in)
 */
export function search(docs, matcher, opts = {}) {
  const { scope = 'plan', words = 10, section = null } = opts
  if (!SCOPES.has(scope)) throw new Error(`Unknown scope: ${scope}`)
  const { ast, terms } = matcher
  if (!ast) return []
  const excl = exclusions(terms, opts.excludedForms)
  const prefixed = section != null || terms.some((t) => t.section)
  const perSection = scope === 'section' || (scope !== 'plan' && prefixed)
  const units = scope !== 'plan'
  const out = []

  for (const doc of docs) {
    const texts = new Array(terms.length)
    const present = new Int8Array(terms.length).fill(-1)
    const tx = (t) => (texts[t.id] ??= textsOf(t, doc, section, perSection))
    const has = (t) => (present[t.id] === -1 ? (present[t.id] = count(t, tx(t), excl[t.id], true) ? 1 : 0) : present[t.id]) === 1
    const evalPlan = (node) =>
      node.type === 'term'
        ? has(node)
        : node.type === 'not'
          ? !evalPlan(node.child)
          : node.type === 'and'
            ? node.children.every(evalPlan)
            : node.children.some(evalPlan)
    if (!evalPlan(ast)) continue
    if (units && !unitMatch(ast, terms, doc, tx, excl, scope, words, evalPlan)) continue
    out.push({ id: doc.id, counts: terms.map((t) => (present[t.id] === 0 ? 0 : count(t, tx(t), excl[t.id], false))) })
  }
  return out
}

// Is there one unit (section, line, sentence, window of words) in which the
// positive terms satisfy the query? NOT parts are evaluated on the plan.
// The hits of all positive terms are walked in text order, so that the walk
// stops at the first unit that satisfies the query.
function unitMatch(ast, terms, doc, tx, excl, scope, words, evalPlan) {
  const notCache = new Map()
  const evalUnit = (node, inUnit) => {
    if (node.type === 'term') return inUnit(node)
    if (node.type === 'not') {
      if (!notCache.has(node)) notCache.set(node, !evalPlan(node.child))
      return notCache.get(node)
    }
    return node.type === 'and' ? node.children.every((c) => evalUnit(c, inUnit)) : node.children.some((c) => evalUnit(c, inUnit))
  }
  if (evalUnit(ast, () => false)) return true // satisfied by NOT parts alone

  // hits by text, each coded as start * M + term id
  const M = terms.length
  const byKey = new Map()
  for (const t of terms) {
    if (t.negated) continue
    const ex = excl[t.id]
    for (const [key, text, lc] of tx(t)) {
      let e = byKey.get(key)
      if (!e) byKey.set(key, (e = { text, lc, codes: [] }))
      t.scan(text, lc, (s, end) => {
        if (!ex || !ex.has(formAt(lc, s, end))) e.codes.push(s * M + t.id)
        return false
      })
    }
  }

  const flag = new Uint8Array(M)
  const inFlag = (t) => flag[t.id] === 1
  for (const { text, lc, codes } of byKey.values()) {
    if (scope === 'section') {
      flag.fill(0)
      for (const c of codes) flag[c % M] = 1
      if (evalUnit(ast, inFlag)) return true
      continue
    }
    const sorted = Float64Array.from(codes).sort()
    if (scope === 'words') {
      // last[t] = word index of the latest hit of term t; a hit at word w
      // completes a match if the terms it needs were seen at w - words or later
      const last = new Float64Array(M).fill(-Infinity)
      const near = (t) => last[t.id] >= wIdx - words
      let wIdx = 0
      let nStarts = 0 // word starts before pos (an inner hyphen starts none)
      let pos = 0
      let pw = 0 // char before pos is a word char
      let ph = false // char before pos is a hyphen after a word char
      for (const c of sorted) {
        const s = Math.floor(c / M)
        for (; pos <= s; pos++) {
          const ch = lc.charCodeAt(pos)
          const w = WORD[ch]
          if (w === 1 && pw === 0 && !ph) nStarts++
          ph = ch === 45 && pw === 1
          pw = w
        }
        wIdx = Math.max(0, nStarts - 1)
        last[c % M] = wIdx
        if (evalUnit(ast, near)) return true
      }
      continue
    }
    let unit = -1
    for (const c of sorted) {
      const s = Math.floor(c / M)
      const u = scope === 'paragraph' ? lineStart(text, s) : sentenceStart(text, lc, s)
      if (u !== unit) {
        flag.fill(0)
        unit = u
      }
      const id = c % M
      if (!flag[id]) {
        flag[id] = 1
        if (evalUnit(ast, inFlag)) return true
      }
    }
  }
  return false
}

/**
 * The words each term's matches fall in (see `formAt`), over all `docs`
 * (excluded forms are listed too, so that they can be ticked again).
 * @returns {{id, key, forms: {form: string, plans: number, count: number}[]}[]}
 *   one entry per term, forms sorted by count, then plans, then form
 */
export function wordForms(docs, matcher, opts = {}) {
  const { section = null } = opts
  return matcher.terms.map((t) => {
    const forms = new Map()
    for (const doc of docs) {
      const seen = new Set()
      for (const [, text, lc] of textsOf(t, doc, section, false))
        t.scan(text, lc, (s, e) => {
          const f = formAt(lc, s, e)
          let r = forms.get(f)
          if (!r) forms.set(f, (r = { form: f, plans: 0, count: 0 }))
          r.count++
          if (!seen.has(f)) {
            seen.add(f)
            r.plans++
          }
          return false
        })
    }
    const list = [...forms.values()].sort(
      (a, b) => b.count - a.count || b.plans - a.plans || (a.form < b.form ? -1 : a.form > b.form ? 1 : 0),
    )
    return { id: t.id, key: t.key, forms: list }
  })
}

const squash = (s) => s.replace(/\s+/g, ' ')

/**
 * Keyword in context for one plan: the matches of the positive terms, in
 * the plan text (or a term's section), in text order.
 * @param {object} doc one of `prepareDocs`
 * @param {object} [opts] `context` (characters on each side, default 60),
 *   `max` (default 20), `section`, `excludedForms`
 * @returns {{term: number, source: string, start: number, end: number,
 *   before: string, match: string, after: string}[]} `source` is '' for the
 *   plan text or a section code; `start`/`end` are offsets into that original
 *   text; before/match/after have whitespace collapsed
 */
export function kwic(doc, matcher, opts = {}) {
  const { context = 60, max = 20, section = null } = opts
  const excl = exclusions(matcher.terms, opts.excludedForms)
  const hits = []
  for (const t of matcher.terms) {
    if (t.negated) continue
    for (const [key, s, e, text] of hitList(t, textsOf(t, doc, section, false), excl[t.id]))
      hits.push({ term: t.id, source: key, start: s, end: e, text })
  }
  hits.sort((a, b) => (a.source < b.source ? -1 : a.source > b.source ? 1 : a.start - b.start || a.term - b.term))
  return hits.slice(0, max).map(({ text, ...h }) => ({
    ...h,
    before: squash(text.slice(Math.max(0, h.start - context), h.start)),
    match: squash(text.slice(h.start, h.end)),
    after: squash(text.slice(h.end, h.end + context)),
  }))
}

/**
 * Ranges to mark in a text: the matches of the positive terms, overlapping
 * ones merged.
 * @param {string} text the plan text, or a section's text
 * @param {object} [opts] `section`: the section code when `text` is a section
 *   (terms with another section's prefix are then left out); `excludedForms`
 * @returns {{start: number, end: number, terms: number[]}[]} sorted, disjoint
 */
export function highlights(text, matcher, opts = {}) {
  const { section = null } = opts
  const excl = exclusions(matcher.terms, opts.excludedForms)
  const lc = fold(text)
  const hits = []
  for (const t of matcher.terms) {
    if (t.negated || (section && t.section && t.section !== section)) continue
    for (const [, s, e] of hitList(t, [['', text, lc]], excl[t.id])) hits.push([s, e, t.id])
  }
  hits.sort((a, b) => a[0] - b[0] || b[1] - a[1])
  const out = []
  for (const [s, e, id] of hits) {
    const last = out[out.length - 1]
    if (last && s < last.end) {
      last.end = Math.max(last.end, e)
      if (!last.terms.includes(id)) last.terms.push(id)
    } else out.push({ start: s, end: e, terms: [id] })
  }
  return out
}
