// Courses, course-years and plan versions, and the filtered, matched view of
// them. Pure functions; counts are of courses and course-years, never of DBH
// rows (README "Decisions").

import { courseKey, planKey } from './keys.js'

const cmp = (a, b) => (a < b ? -1 : a > b ? 1 : 0)

/**
 * Index courses with their course-years (the course_years rows collapsed to
 * one per course and year).
 * @returns {{courses, byKey: Map, courseYears, yearMin: number, yearMax: number}}
 *   each course: its row plus `key`, `years` (Map year -> course-year) and
 *   `versions` (empty until `addPlans`); each course-year: `{year,
 *   studyTypes: Set, plans: Set of plan keys}`
 */
export function buildModel({ courses, courseYears }) {
  const byKey = new Map()
  const list = courses
    .map((c) => ({ ...c, key: courseKey(c), years: new Map(), versions: [] }))
    .sort((a, b) => cmp(a.institution, b.institution) || cmp(a.course_code, b.course_code))
  for (const c of list) byKey.set(c.key, c)
  const cys = []
  let yearMin = Infinity
  let yearMax = -Infinity
  for (const r of courseYears) {
    const c = byKey.get(courseKey(r))
    if (!c) continue
    let cy = c.years.get(r.year)
    if (!cy) {
      cy = { year: r.year, studyTypes: new Set(), plans: new Set() }
      c.years.set(r.year, cy)
      cys.push(cy)
      if (r.year < yearMin) yearMin = r.year
      if (r.year > yearMax) yearMax = r.year
    }
    if (r.study_type) cy.studyTypes.add(r.study_type)
    if (r.plan_content_id) cy.plans.add(planKey(r))
  }
  return { courses: list, byKey, courseYears: cys, yearMin, yearMax }
}

/**
 * A copy of the model with the plan versions (plans rows without text)
 * attached to new course objects, latest version first.
 */
export function addPlans(model, plans) {
  const by = new Map()
  for (const p of plans) {
    const k = courseKey(p)
    if (!by.has(k)) by.set(k, [])
    by.get(k).push({ ...p, key: planKey(p) })
  }
  const courses = model.courses.map((c) => ({
    ...c,
    versions: (by.get(c.key) ?? []).sort((a, b) => b.year_from - a.year_from || b.year_to - a.year_to),
  }))
  return { ...model, courses, byKey: new Map(courses.map((c) => [c.key, c])) }
}

const overlaps = (v, y0, y1) => v.year_from <= y1 && v.year_to >= y0
const sum = (a) => a.reduce((s, x) => s + x, 0)

/**
 * The courses in the filter that match, with counts.
 * @param {object} model from `buildModel`
 * @param {{inst: string[], y0: number|null, y1: number|null, st: string[]}} f
 * @param {Map<string, number[]>|null} matches plan key -> hit counts per term;
 *   null without a query (every course in the filter matches)
 * @returns {{rows: {course, versions, hits: number}[], counts, instCounts: Map}}
 *   rows: the matching courses in model order, `versions` the matching ones
 *   whose years overlap the filter (latest first), `hits` the hits in the
 *   latest of them; counts: courses, versions, courseYears (in the filter),
 *   withPlan, matching (course-years with a matching plan); instCounts:
 *   matching courses per institution, ignoring the institution filter
 */
export function filterView(model, f, matches) {
  const y0 = f.y0 ?? model.yearMin
  const y1 = f.y1 ?? model.yearMax
  const inst = f.inst.length ? new Set(f.inst) : null
  const st = f.st.length ? new Set(f.st) : null
  const cyIn = (cy) => cy.year >= y0 && cy.year <= y1 && (!st || [...cy.studyTypes].some((s) => st.has(s)))
  const rows = []
  const counts = { courses: 0, versions: 0, courseYears: 0, withPlan: 0, matching: 0 }
  const instCounts = new Map()
  for (const c of model.courses) {
    let n = 0
    let withPlan = 0
    let matching = 0
    for (const cy of c.years.values()) {
      if (!cyIn(cy)) continue
      n++
      if (cy.plans.size) withPlan++
      if (matches && [...cy.plans].some((k) => matches.has(k))) matching++
    }
    if (!n) continue
    const versions = c.versions.filter((v) => overlaps(v, y0, y1) && (!matches || matches.has(v.key)))
    const hit = matches ? versions.length > 0 : true
    if (hit) instCounts.set(c.institution, (instCounts.get(c.institution) ?? 0) + 1)
    if (inst && !inst.has(c.institution)) continue
    counts.courseYears += n
    counts.withPlan += withPlan
    counts.matching += matching
    if (!hit) continue
    counts.courses++
    counts.versions += versions.length
    rows.push({ course: c, versions, hits: matches && versions.length ? sum(matches.get(versions[0].key)) : 0 })
  }
  return { rows, counts, instCounts }
}

/**
 * The year strip of a course: one cell per year of [y0, y1], with state
 * 'match' (a matching version in force), 'plan' (a plan, none matching),
 * 'none' (taught without a plan) or '' (not taught, no plan).
 * @param {Set<string>|null} matched keys of the course's matching versions
 */
export function yearStrip(course, y0, y1, matched) {
  const out = []
  for (let y = y0; y <= y1; y++) {
    const inForce = course.versions.filter((v) => v.year_from <= y && v.year_to >= y)
    const cy = course.years.get(y)
    let state = ''
    if (matched && inForce.some((v) => matched.has(v.key))) state = 'match'
    else if (inForce.length || cy?.plans.size) state = 'plan'
    else if (cy) state = 'none'
    out.push({ year: y, state })
  }
  return out
}

/** The version to show: the requested one, else the latest matching, else the latest. */
export function pickVersion(course, row, versionId, idOf) {
  return (
    (versionId && course.versions.find((v) => idOf(v) === versionId)) || row?.versions[0] || course.versions[0] || null
  )
}
