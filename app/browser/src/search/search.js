// Search over prepared plans: matching, word forms, keyword in context and
// highlighting. Pure functions without DOM, for use in a Web Worker.

import { fold, FoldedText, formAt, isSpace, WORD } from './text.js'

const folded = (t) => (t instanceof FoldedText ? t : new FoldedText(t))

/**
 * Prepare plans for searching: each text as a `FoldedText` (its folded copy,
 * whose offsets equal those of the original, and the original on demand).
 * @param {{id: any, text: string|FoldedText, sections?: Object<string, string|FoldedText>}[]} plans
 * @returns {FoldedText[]} each with `id` and `sections` ({code: FoldedText});
 *   the worker adds sections later
 */
export function prepareDocs(plans) {
  return plans.map((p) => {
    const doc = folded(p.text ?? '')
    doc.id = p.id
    doc.sections = {}
    for (const [code, t] of Object.entries(p.sections ?? {})) {
      const f = t && folded(t)
      if (f?.lc) doc.sections[code] = f
    }
    return doc
  })
}

// Run a term's scanner over a FoldedText: a regular expression runs on the
// folded copy where that is exact, the other scanners read only that copy.
const scanIn = (term, src, hit) => term.scan(term.kind === 'regex' && !src.regexOnLc ? src.text : src.lc, src.lc, hit)

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

// The texts a term is matched in, as [key, FoldedText]: its section (prefix,
// or the search's section), else the plan text; in the section-based scopes
// an unprefixed positive term is matched in every section.
function textsOf(term, doc, section, perSection) {
  const code = term.section ?? section
  if (code) {
    const s = doc.sections[code]
    return s ? [[code, s]] : []
  }
  if (perSection && !term.negated) return Object.entries(doc.sections)
  return [['', doc]]
}

// Count the matches of a term in texts, without those inside excluded forms;
// `first`: stop at the first.
function count(term, texts, excl, first) {
  let n = 0
  for (const [, src] of texts) {
    const lc = src.lc
    scanIn(term, src, (s, e) => {
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
  for (const [key, src] of texts)
    scanIn(term, src, (s, e) => {
      if (!excl || !excl.has(formAt(src.lc, s, e))) out.push([key, s, e, src])
      return false
    })
  return out
}

// Start of the unit (line or sentence) that position s falls in (folding
// leaves line breaks, punctuation, spaces and digits as they are).
function lineStart(lc, s) {
  return lc.lastIndexOf('\n', s - 1) + 1
}
function sentenceStart(src, s) {
  const lc = src.lc
  for (let i = s - 1; i >= 0; i--) {
    const c = lc[i]
    if (c === '\n') return i + 1
    if ((c === '.' || c === '!' || c === '?') && isSpace(lc, i + 1)) {
      let j = i + 1
      while (j < s && isSpace(lc, j)) j++
      if (j <= s && (src.differs(j) || /\d/.test(lc[j]))) return j
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
    for (const [key, src] of tx(t)) {
      let e = byKey.get(key)
      if (!e) byKey.set(key, (e = { src, codes: [] }))
      const lc = src.lc
      scanIn(t, src, (s, end) => {
        if (!ex || !ex.has(formAt(lc, s, end))) e.codes.push(s * M + t.id)
        return false
      })
    }
  }

  const flag = new Uint8Array(M)
  const inFlag = (t) => flag[t.id] === 1
  for (const { src, codes } of byKey.values()) {
    const lc = src.lc
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
      const u = scope === 'paragraph' ? lineStart(lc, s) : sentenceStart(src, s)
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
      for (const [, src] of textsOf(t, doc, section, false))
        scanIn(t, src, (s, e) => {
          const f = formAt(src.lc, s, e)
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
    return { id: t.id, key: t.key, forms: sortForms([...forms.values()]) }
  })
}

const sortForms = (list) =>
  list.sort((a, b) => b.count - a.count || b.plans - a.plans || (a.form < b.form ? -1 : a.form > b.form ? 1 : 0))

/** The word forms of disjoint sets of plans (`wordForms` of each) as one. */
export function mergeForms(parts) {
  if (parts.length === 1) return parts[0]
  return parts[0].map((t, i) => {
    const forms = new Map()
    for (const p of parts)
      for (const f of p[i].forms) {
        const r = forms.get(f.form)
        if (r) {
          r.plans += f.plans
          r.count += f.count
        } else forms.set(f.form, { ...f })
      }
    return { ...t, forms: sortForms([...forms.values()]) }
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
    for (const [key, s, e, src] of hitList(t, textsOf(t, doc, section, false), excl[t.id]))
      hits.push({ term: t.id, source: key, start: s, end: e, src })
  }
  hits.sort((a, b) => (a.source < b.source ? -1 : a.source > b.source ? 1 : a.start - b.start || a.term - b.term))
  return hits.slice(0, max).map(({ src, ...h }) => ({
    ...h,
    before: squash(src.slice(Math.max(0, h.start - context), h.start)),
    match: squash(src.slice(h.start, h.end)),
    after: squash(src.slice(h.end, h.end + context)),
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
  const src = new FoldedText(text)
  const hits = []
  for (const t of matcher.terms) {
    if (t.negated || (section && t.section && t.section !== section)) continue
    for (const [, s, e] of hitList(t, [['', src]], excl[t.id])) hits.push([s, e, t.id])
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
