// The data in shards (README "Data Contract", "Shards"): the all-courses data
// are split by study type and a shard is loaded when its type is ticked. Data
// without `meta.shards` (the teacher-ed frame) are one shard, in the data
// folder itself, loaded at start. A course is whole in every shard of its
// study types, so the first copy loaded is kept and later ones are skipped.

import { courseKey } from './keys.js'

/** The data sets: the teacher-ed frame in `data/`, all courses in `data/all/`. */
export const DATASETS = { te: 'data/', all: 'data/all/' }

/** The shards to load: those of the ticked study types, or the one shard of
 *  unsharded data. Each: {code, dir} (dir relative to the data folder, with
 *  a trailing slash, or '' for the folder itself). */
export function shardsFor(meta, st) {
  if (!meta.shards) return [{ code: '', dir: '' }]
  const on = new Set(st)
  return meta.shards.filter((s) => on.has(s.code)).map((s) => ({ code: s.code, dir: `${s.dir}/` }))
}

const programmeKey = (p) => `${p.institution}|${p.programme_code}`

/**
 * The tables loaded so far plus a shard's, without the courses (and their
 * course-years) or programmes already loaded.
 * @param {{courses, courseYears, programmes}} acc
 * @param {{courses, courseYears, programmes}} shard
 */
export function mergeShard(acc, shard) {
  const seen = new Set(acc.courses.map(courseKey))
  const courses = shard.courses.filter((c) => !seen.has(courseKey(c)))
  const fresh = new Set(courses.map(courseKey))
  const progs = new Set(acc.programmes.map(programmeKey))
  return {
    courses: acc.courses.concat(courses),
    courseYears: acc.courseYears.concat(shard.courseYears.filter((r) => fresh.has(courseKey(r)))),
    programmes: acc.programmes.concat(shard.programmes.filter((p) => !progs.has(programmeKey(p)))),
  }
}
