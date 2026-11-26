// The trend and coverage views: course-years in the filter counted by year
// and group. A course-year counts once per group however many programmes and
// plans it has; it matches when any of its plans matches; the share is of the
// course-years with a plan, and coverage of all course-years (README
// "Decisions"). Pure functions.

import { yearFilter } from './model.js'
import { COURSE_FILTERS } from './filters.js'

export const GROUPS = ['all', 'inst', 'st']
export const MAX_SERIES = 8
export const OTHERS = '(others)'

export const blank = (year) => ({ year, n: 0, withPlan: 0, matching: 0 })

/** Count a course-year in a point: in force, with a plan, matching (any of
 *  its plans in `matches`, a Map or Set of plan keys). */
export function add(p, cy, matches) {
  p.n++
  if (!cy.plans.size) return
  p.withPlan++
  if (matches && [...cy.plans].some((k) => matches.has(k))) p.matching++
}

/** The course-years in the filter (the course filters, years and study
 *  types), each with its course: calls `fn(course, courseYear)`. */
export function eachCourseYear(model, f, fn) {
  const { cyIn } = yearFilter(model, f)
  const tests = Object.entries(COURSE_FILTERS)
    .filter(([k]) => f[k]?.length)
    .map(([k, get]) => [get, new Set(f[k])])
  for (const c of model.courses) {
    if (tests.some(([get, vals]) => !vals.has(get(c)))) continue
    for (const cy of c.years.values()) if (cyIn(cy)) fn(c, cy)
  }
}

/**
 * Counts by year, overall and per group.
 * @param {'all'|'inst'|'st'} by group by institution or study type (a
 *   course-year counts in each of its study types in the filter), or none
 * @param {Map|null} matches plan key -> hits; null without a query
 * @returns {{years: number[], total: object[], series: {key, points: object[], other?: true}[]}}
 *   points (one per year of the filter): `{year, n, withPlan, matching}`;
 *   series sorted by key; past MAX_SERIES groups the largest MAX_SERIES - 1
 *   (by course-years with a plan) and OTHERS: the course-years of any other
 *   group, once each
 */
export function trend(model, f, matches, by = 'all') {
  const { y0, y1 } = yearFilter(model, f)
  const years = []
  for (let y = y0; y <= y1; y++) years.push(y)
  const row = () => years.map(blank)
  const total = row()
  const st = f.st.length ? new Set(f.st) : null
  const groupsOf = by === 'inst' ? (c) => [c.institution] : by === 'st' ? (c, cy) => [...cy.studyTypes].filter((s) => !st || st.has(s)) : () => []
  const items = []
  const size = new Map()
  eachCourseYear(model, f, (c, cy) => {
    add(total[cy.year - y0], cy, matches)
    const g = groupsOf(c, cy)
    if (!g.length) return
    items.push([g, cy])
    if (cy.plans.size) for (const k of g) size.set(k, (size.get(k) ?? 0) + 1)
    for (const k of g) if (!size.has(k)) size.set(k, 0)
  })
  const keys = [...size.keys()].sort((a, b) => size.get(b) - size.get(a) || (a < b ? -1 : 1))
  const shown = new Set(keys.length > MAX_SERIES ? keys.slice(0, MAX_SERIES - 1) : keys)
  const pts = new Map([...shown].map((k) => [k, row()]))
  let others = null
  if (keys.length > shown.size) others = row()
  for (const [g, cy] of items) {
    for (const k of g) if (shown.has(k)) add(pts.get(k)[cy.year - y0], cy, matches)
    if (others && g.some((k) => !shown.has(k))) add(others[cy.year - y0], cy, matches)
  }
  const series = [...shown].sort().map((key) => ({ key, points: pts.get(key) }))
  if (others) series.push({ key: OTHERS, points: others, other: true })
  return { years, total, series }
}

/**
 * Coverage per institution in the filter: course-years, with a plan, and the
 * no-plan reasons (`no_plan_reason`, or null where none is recorded) by
 * count, largest first.
 * @returns {{institution, n, withPlan, reasons: [string|null, number][]}[]} sorted by institution
 */
export function coverage(model, f) {
  const by = new Map()
  eachCourseYear(model, f, (c, cy) => {
    let r = by.get(c.institution)
    if (!r) by.set(c.institution, (r = { institution: c.institution, n: 0, withPlan: 0, reasons: new Map() }))
    r.n++
    if (cy.plans.size) r.withPlan++
    else r.reasons.set(cy.noPlanReason, (r.reasons.get(cy.noPlanReason) ?? 0) + 1)
  })
  return [...by.values()]
    .sort((a, b) => (a.institution < b.institution ? -1 : 1))
    .map((r) => ({ ...r, reasons: [...r.reasons].sort((a, b) => b[1] - a[1]) }))
}

/**
 * The sections the plans have, per institution in the filter: the plan
 * versions of the course-years in the filter (each version once), and of
 * them how many have each section (what a section search can see).
 * @param {Map<string, Set<string>>} planSections plan key -> its sections
 * @returns {{rows: Map<string, {plans: number, has: Map<string, number>}>, total: {plans, has}}}
 *   rows by institution; a plan without sections counts in `plans` only
 */
export function sectionCoverage(model, f, planSections) {
  const keys = new Map() // institution -> Set of plan keys
  eachCourseYear(model, f, (c, cy) => {
    if (!cy.plans.size) return
    let k = keys.get(c.institution)
    if (!k) keys.set(c.institution, (k = new Set()))
    for (const p of cy.plans) k.add(p)
  })
  const blankRow = () => ({ plans: 0, has: new Map() })
  const total = blankRow()
  const rows = new Map()
  for (const [inst, ks] of keys) {
    const r = blankRow()
    rows.set(inst, r)
    for (const k of ks) {
      r.plans++
      total.plans++
      for (const code of planSections.get(k) ?? []) {
        r.has.set(code, (r.has.get(code) ?? 0) + 1)
        total.has.set(code, (total.has.get(code) ?? 0) + 1)
      }
    }
  }
  return { rows, total }
}

/** Share of `a` in `b`, null when `b` is 0. */
export const share = (a, b) => (b ? a / b : null)
