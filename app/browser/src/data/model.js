// Courses, course-years and plan versions, and the filtered, matched view of
// them. Pure functions; counts are of courses and course-years, never of DBH
// rows (README "Decisions").

import { courseKey, planKey } from './keys.js'
import { COURSE_FILTERS } from './filters.js'

const cmp = (a, b) => (a < b ? -1 : a > b ? 1 : 0)

/**
 * Index courses with their course-years (the course_years rows collapsed to
 * one per course and year).
 * @returns {{courses, byKey: Map, courseYears, yearMin: number, yearMax: number, yearFrom: number}}
 *   yearFrom: the first year with a plan, the default start of the year filter
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
  let yearFrom = Infinity
  for (const r of courseYears) {
    const c = byKey.get(courseKey(r))
    if (!c) continue
    let cy = c.years.get(r.year)
    if (!cy) {
      cy = { year: r.year, studyTypes: new Set(), plans: new Set(), programmes: new Set(), hadStudents: false, noPlanReason: null }
      c.years.set(r.year, cy)
      cys.push(cy)
      if (r.year < yearMin) yearMin = r.year
      if (r.year > yearMax) yearMax = r.year
    }
    if (r.study_type) cy.studyTypes.add(r.study_type)
    if (r.plan_content_id) {
      cy.plans.add(planKey(r))
      if (r.year < yearFrom) yearFrom = r.year
    }
    if (r.programme_code) cy.programmes.add(r.programme_code)
    if (r.had_students) cy.hadStudents = true
    if (r.no_plan_reason) cy.noPlanReason = r.no_plan_reason
  }
  if (yearFrom > yearMax) yearFrom = yearMin
  return { courses: list, byKey, courseYears: cys, yearMin, yearMax, yearFrom }
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

/** The year range of the filter and whether a course-year is in the filter
 *  (years and study types). */
export function yearFilter(model, f) {
  const y0 = f.y0 ?? model.yearFrom
  const y1 = f.y1 ?? model.yearMax
  const st = f.st.length ? new Set(f.st) : null
  return { y0, y1, cyIn: (cy) => cy.year >= y0 && cy.year <= y1 && (!st || [...cy.studyTypes].some((s) => st.has(s))) }
}

const sum = (a) => a.reduce((s, x) => s + x, 0)

/**
 * The courses in the filter that match, with counts.
 * @param {object} model from `buildModel`
 * @param {{inst, y0, y1, st, subj, nus, lvl, cr, lang, sec}} f the filters (url.js);
 *   `sec`: sections a version must all have
 * @param {Map<string, number[]>|null} matches plan key -> hit counts per term;
 *   null without a query (every course in the filter matches)
 * @param {Map<string, Set<string>>|null} planSections plan key -> its
 *   sections; null until loaded (the section filter waits for it)
 * @returns {{rows: {course, versions, hits: number}[], counts, instCounts: Map, facets}}
 *   rows: the matching courses in model order, `versions` the matching ones
 *   (query and sections) whose years overlap the filter (latest first), `hits`
 *   the hits in the latest of them; counts: courses, versions, courseYears (in
 *   the filter), withPlan, matching (course-years with a matching plan);
 *   facets: for each filter, value -> matching courses with every other filter
 *   applied but its own (`st`: code -> {courses, courseYears, withPlan}, the
 *   course-years of the type in the years, of the courses in the other
 *   filters whether they match or not); instCounts: facets.inst
 */
export function filterView(model, f, matches, planSections = null) {
  const { y0, y1 } = yearFilter(model, f)
  const st = f.st.length ? new Set(f.st) : null
  const attrs = Object.entries(COURSE_FILTERS).map(([k, get]) => ({ k, get, set: f[k]?.length ? new Set(f[k]) : null }))
  const sec = f.sec?.length && planSections ? f.sec : null
  const planOk = (k) => (!matches || matches.has(k)) && (!sec || sec.every((s) => planSections.get(k)?.has(s)))
  const rows = []
  const counts = { courses: 0, versions: 0, courseYears: 0, withPlan: 0, matching: 0 }
  const facets = Object.fromEntries(['st', 'sec', ...attrs.map((a) => a.k)].map((k) => [k, new Map()]))
  const add = (m, v) => m.set(v, (m.get(v) ?? 0) + 1)
  for (const c of model.courses) {
    const vals = attrs.map((a) => a.get(c))
    const fails = attrs.filter((a, i) => a.set && !a.set.has(vals[i]))
    if (fails.length > 1) continue
    let n = 0
    let withPlan = 0
    let matching = 0
    const types = new Map() // study type -> [course-years, with a plan] in the years
    for (const cy of c.years.values()) {
      if (cy.year < y0 || cy.year > y1) continue
      for (const t of cy.studyTypes) {
        const x = types.get(t) ?? types.set(t, [0, 0]).get(t)
        x[0]++
        if (cy.plans.size) x[1]++
      }
      if (st && ![...cy.studyTypes].some((t) => st.has(t))) continue
      n++
      if (cy.plans.size) withPlan++
      if ((matches || sec) && [...cy.plans].some(planOk)) matching++
    }
    const inYears = c.versions.filter((v) => overlaps(v, y0, y1) && (!matches || matches.has(v.key)))
    const versions = sec ? inYears.filter((v) => planOk(v.key)) : inYears
    const hit = matches || sec ? versions.length > 0 : true
    if (n && hit) attrs.forEach((a, i) => (!fails.length || fails[0] === a) && add(facets[a.k], vals[i]))
    if (fails.length) continue
    for (const [t, [cys, wp]] of types) {
      const x = facets.st.get(t) ?? facets.st.set(t, { courses: 0, courseYears: 0, withPlan: 0 }).get(t)
      x.courseYears += cys
      x.withPlan += wp
      if (hit) x.courses++
    }
    if (!n) continue
    if (planSections) new Set(inYears.flatMap((v) => [...(planSections.get(v.key) ?? [])])).forEach((s) => add(facets.sec, s))
    counts.courseYears += n
    counts.withPlan += withPlan
    counts.matching += matching
    if (!hit) continue
    counts.courses++
    counts.versions += versions.length
    rows.push({ course: c, versions, hits: matches && versions.length ? sum(matches.get(versions[0].key)) : 0 })
  }
  return { rows, counts, instCounts: facets.inst, facets }
}

/**
 * The year strip of a course: one cell per year of [y0, y1], with state
 * 'match' (a matching version in force), 'plan' (a plan, none matching),
 * 'none' (taught without a plan; `reason` its no_plan_reason) or '' (not
 * taught, no plan).
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
    out.push({ year: y, state, reason: state === 'none' ? cy.noPlanReason : null })
  }
  return out
}

/** The version to show: the requested one, else the latest matching, else the latest. */
export function pickVersion(course, row, versionId, idOf) {
  return (
    (versionId && course.versions.find((v) => idOf(v) === versionId)) || row?.versions[0] || course.versions[0] || null
  )
}
