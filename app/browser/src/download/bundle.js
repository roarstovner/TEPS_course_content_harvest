// A download as one ZIP: the table (CSV, XLSX or Parquet) or the QDA files,
// with manifest.json, variables.csv, the keys of its rows and R and Python
// code that rebuilds them from the published release. Single-file formats are
// zipped too, so that a table never travels without its manifest.

import { toCsv, toParquet, toXlsx, truncateForExcel, parseCsv } from './encode.js'
import { keyRow, UNITS } from './table.js'
import { rebuildPy, rebuildR } from './rebuild.js'

// Columns of the release files: [file in data/variables.csv]; browser-only
// columns: [null, type, description].
const LIST = 'separated by "; "'
const COLUMNS = {
  institution: ['course_offerings'],
  institution_name: [null, 'character', "The institution's latest name in DBH."],
  course_code: ['course_offerings'],
  course_name: [null, 'character', 'Course name in its latest offering in DBH.'],
  level_code: ['course_offerings'],
  credits: ['course_offerings'],
  language_code: ['course_offerings'],
  subject_code: ['course_offerings'],
  subject_name: ['course_offerings'],
  nus_code: ['course_offerings'],
  department_name: [null, 'character', 'Unit that offers the course, in its latest offering.'],
  year_first: [null, 'integer', 'First year DBH lists the course in the frame.'],
  year_last: [null, 'integer', 'Last year DBH lists the course in the frame.'],
  year: ['course_offerings'],
  study_types: [
    null,
    'character',
    `DBH study types (table 347 Studiumkode) of the programmes DBH lists the course under in the selected years (of a plan: in the course-years that have it), ${LIST}.`,
  ],
  programme_codes: [null, 'character', `The programmes (programme_code) DBH lists the course under that year, ${LIST}.`],
  course_years: [null, 'integer', 'Course-years of the course in the filter (years and study types).'],
  course_years_with_plan: [null, 'integer', 'Of these, course-years with a plan.'],
  course_years_matching: [null, 'integer', 'Of these, course-years with a plan that matches the query; NA without a query.'],
  plan_versions: [null, 'integer', 'Plan versions of the course in the selection (matching the query, in force in the selected years).'],
  hits: [
    null,
    'integer',
    'Matches of the query (positive terms, unticked word forms left out) in the plan, the section or the selected versions; NA without a query.',
  ],
  had_students: ['course_offerings'],
  has_plan: [null, 'logical', 'Whether the course-year has a plan.'],
  plan_ids: [null, 'character', `The plans (plan_id) of the course-year, ${LIST}.`],
  matching: [null, 'logical', 'Whether a plan of the course-year matches the query; NA without a query.'],
  no_plan_reason: [
    null,
    'character',
    'Why the course-year has no plan (reason in data/interim/plan_gaps.RDS, see data/data_notes.qmd); NA with a plan.',
  ],
  plan_id: ['course_plans'],
  plan_content_id: ['course_plans'],
  year_from: ['course_plans'],
  year_to: ['course_plans'],
  source_url: [null, 'character', 'The page the plan was read from.'],
  harvested_at: ['course_offerings', 'character', null],
  course_plan: ['course_plans'],
  section: ['plan_sections'],
  text: ['plan_sections'],
  term: [null, 'character', 'The query term that matched, as written (lower-cased, with its section prefix).'],
  position: [null, 'integer', "Character offset (from 0) of the match in the plan text, or in the section's text when section is set."],
  before: [null, 'character', 'Up to 80 characters before the match, whitespace squished.'],
  match: [null, 'character', 'The matched text.'],
  after: [null, 'character', 'Up to 80 characters after the match.'],
  truncated: [null, 'logical', "XLSX only: whether a text cell was cut at Excel's limit of 32,767 characters (the CSV and Parquet downloads hold the whole text)."],
  document: [null, 'character', 'QDA: name of the text file without .txt (the document name in NVivo, MAXQDA and ATLAS.ti).'],
}

/**
 * Rows of variables.csv for the columns of a table.
 * @param {string} dictionary text of data/variables.csv
 * @returns {{variable, type, source, description}[]}
 */
export function variables(cols, dictionary) {
  const dict = new Map(parseCsv(dictionary).map((r) => [`${r.file}.${r.variable}`, r]))
  return cols.map((c) => {
    const [file, type, desc] = COLUMNS[c] ?? [null, 'character', '']
    const d = file ? dict.get(`${file}.${c}`) : null
    return {
      variable: c,
      type: type ?? d?.type ?? 'character',
      source: file ? `${file} (release)` : 'browser',
      description: desc ?? d?.description ?? '',
    }
  })
}

/** R type of each column, for Parquet. */
export const typesOf = (vars) => Object.fromEntries(vars.map((v) => [v.variable, v.type]))

const slug = (s) =>
  String(s)
    .replace(/[^\p{L}\p{N}._-]+/gu, '-')
    .replace(/^-+|-+$/g, '')

const stamp = (d) => `${d.toISOString().slice(0, 16).replace('T', '_').replace(':', '')}Z`

/** File name of a download: data version, unit and time (UTC, Z). */
export const zipName = (version, unit, date) => `course-plans_${slug(version)}_${unit}_${stamp(date)}.zip`

/**
 * QDA documents: one text file per plan version, named from institution,
 * course code and years (a short id added where two would clash), the plan
 * text under a short header.
 * @param {object[]} plans plan rows with `course_plan`
 * @returns {{name: string, text: string}[]} name without .txt
 */
export function qdaDocs(plans, version) {
  const base = (p) => `${p.institution}_${slug(p.course_code)}_${p.year_from}${p.year_to !== p.year_from ? `-${p.year_to}` : ''}`
  const n = new Map()
  for (const p of plans) n.set(base(p), (n.get(base(p)) ?? 0) + 1)
  return plans.map((p) => {
    const name = n.get(base(p)) > 1 ? `${base(p)}_${p.plan_content_id.slice(0, 6)}` : base(p)
    const years = p.year_to !== p.year_from ? `${p.year_from}-${p.year_to}` : `${p.year_from}`
    const head = [
      `${p.course_code} ${p.course_name ?? ''}`.trim(),
      `${p.institution_name ?? p.institution} (${p.institution})`,
      `Years in force: ${years}`,
      `Plan id: ${p.plan_id}`,
      p.source_url ? `Source: ${p.source_url}` : null,
      `Data version: ${version}`,
    ]
    return { name, text: `﻿${head.filter(Boolean).join('\n')}\n\n${p.course_plan ?? ''}\n` }
  })
}

const QDA_README = `Import into a QDA program

texts/: one UTF-8 text file per plan version. classification.csv and
classification.xlsx: one row per document (column "document" is the file name
without .txt) with its attributes.

- NVivo: Import > Files (the texts folder), then Import > Classification
  Sheets (classification.xlsx) as a file classification.
- MAXQDA: Import > Texts (the texts folder), then Variables > Import Document
  Variables (classification.xlsx); MAXQDA wants "Document group" and
  "Document name" as its first two columns: add a column "Document group"
  before "document" and rename that to "Document name".
- ATLAS.ti: Add Documents (the texts folder), then Import Document Variables
  (classification.xlsx).
`

/**
 * The manifest of a download.
 * @param {object} ctx {q, scope, n, excl, filters: {inst, y0, y1, st, subj, lvl, cr, lang, sec}, url,
 *   meta: {version, frame, built_at}, date}
 */
export function manifest(ctx, unit, format, counts, files) {
  const m = ctx.meta
  return {
    title: 'Course plan browser download',
    created_at: ctx.date.toISOString(),
    data: { version: m.version, frame: m.frame, built_at: m.built_at },
    unit,
    unit_label: UNITS[unit],
    format,
    query: ctx.q ? { q: ctx.q, scope: ctx.scope, ...(ctx.scope === 'words' ? { n: ctx.n } : {}), excluded_forms: ctx.excl } : null,
    filters: {
      institutions: ctx.filters.inst,
      years: [ctx.filters.y0, ctx.filters.y1],
      study_types: ctx.filters.st,
      subjects: ctx.filters.subj ?? [],
      nus_groups: ctx.filters.nus ?? [],
      levels: ctx.filters.lvl ?? [],
      credits: ctx.filters.cr ?? [],
      languages: ctx.filters.lang ?? [],
      sections: ctx.filters.sec ?? [],
    },
    counts,
    url: ctx.url,
    files,
  }
}

/**
 * Build a download.
 * @param {object} o
 * @param {string} o.unit one of UNITS
 * @param {string} o.format one of FORMATS
 * @param {object[]} o.rows the unit's table (`unitRows`), not empty
 * @param {object} o.sel the selection (`selection`), for the keys
 * @param {object} o.ctx see `manifest`
 * @param {string} o.dictionary text of data/variables.csv
 * @returns {Promise<{name: string, bytes: Uint8Array, truncated: number}>}
 */
export async function buildDownload({ unit, format, rows, sel, ctx, dictionary }) {
  const { strToU8, zipSync } = await import('fflate')
  const files = {}
  const add = (path, data, level) => (files[path] = level === 0 ? [data, { level: 0 }] : data)
  let cols = Object.keys(rows[0])
  let truncated = 0
  const flag = unit === 'plan' || unit === 'section'
  const xlsx = async (c, r) => {
    const t = truncateForExcel(c, r, flag)
    truncated = t.truncated
    return { cols: t.cols, bytes: await toXlsx(t.cols, t.rows) }
  }

  if (format === 'qda') {
    if (unit !== 'plan') throw new Error('QDA files are made of plan versions')
    const docs = qdaDocs(rows, ctx.meta.version)
    docs.forEach((d) => add(`texts/${d.name}.txt`, strToU8(d.text)))
    cols = ['document', ...cols.filter((c) => c !== 'course_plan')]
    const sheet = rows.map((r, i) => ({ document: docs[i].name, ...r, course_plan: undefined }))
    add('classification.csv', strToU8(toCsv(cols, sheet)))
    add('classification.xlsx', (await toXlsx(cols, sheet)).slice(), 0)
    add('README.txt', strToU8(QDA_README))
  } else if (format === 'csv') add(`${unit}s.csv`, strToU8(toCsv(cols, rows)))
  else if (format === 'xlsx') {
    const x = await xlsx(cols, rows)
    cols = x.cols
    add(`${unit}s.xlsx`, x.bytes, 0)
  } else if (format === 'parquet') {
    add(`${unit}s.parquet`, new Uint8Array(await toParquet(cols, rows, typesOf(variables(cols, dictionary)))), 0)
  } else throw new Error(`unknown format ${format}`)

  const vars = variables(cols, dictionary)
  add('variables.csv', strToU8(toCsv(['variable', 'type', 'source', 'description'], vars)))
  const byCourseYear = unit === 'course' || unit === 'course_year'
  const keys = byCourseYear ? sel.planKeys : uniqueKeys(sel.plans)
  add('keys.csv', strToU8(toCsv(['plan_id', 'plan_content_id', 'institution', 'course_code'], keys)))
  if (byCourseYear) add('course_year_keys.csv', strToU8(toCsv(['institution', 'course_code', 'year'], sel.courseYears)))
  const expected = unit === 'hit' ? keys.length : rows.length
  add('rebuild.R', strToU8(rebuildR(unit, expected, ctx.meta.version)))
  add('rebuild.py', strToU8(rebuildPy(unit, expected, ctx.meta.version)))
  const counts = {
    rows: rows.length,
    courses: new Set(rows.map((r) => `${r.institution}|${r.course_code}`)).size,
    plan_versions: unit === 'course' || unit === 'course_year' ? keys.length : new Set(rows.map((r) => r.plan_id)).size,
    ...(byCourseYear ? { course_years: sel.courseYears.length } : {}),
    ...(truncated ? { truncated_in_xlsx: truncated } : {}),
  }
  const names = [...Object.keys(files), 'manifest.json'].filter((f) => !f.startsWith('texts/'))
  if (format === 'qda') names.unshift(`texts/ (${rows.length} files)`)
  add('manifest.json', strToU8(`${JSON.stringify(manifest(ctx, unit, format, counts, names), null, 2)}\n`))
  return { name: zipName(ctx.meta.version, `${unit}s`, ctx.date), bytes: zipSync(files, { level: 6 }), truncated }
}

const uniqueKeys = (plans) => [...new Map(plans.map((p) => [p.plan_id, keyRow(p)])).values()]
