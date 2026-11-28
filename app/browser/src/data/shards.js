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

/** Shard data past which a selection asks first, and again at each further
 *  step of this size beyond the level last accepted (user 2026-11-28: a
 *  generic group is ~45 MB of shards and ~600 MB of tab memory). */
export const WARN_BYTES = 30e6
/** Tab memory per byte of shard data, roughly (the worker's heap and the
 *  page's model: ~600 MB for ~45 MB of shards). */
export const MEMORY_FACTOR = 13

/**
 * Which shards of the ticked study types to load now and which to hold for
 * confirmation: all new ones are held when the shards loaded plus these
 * reach a further `WARN_BYTES` step than `accepted` (the bytes last
 * accepted, 0 at first). Unsharded data never ask.
 * @param {object} meta
 * @param {string[]} st the ticked study types
 * @param {string[]} loaded codes of the shards loaded
 * @param {number} accepted
 * @returns {{load: {code, dir}[], hold: {code, dir}[], bytes: number, more: number}}
 *   bytes: all the selection would hold; more: those of the new shards
 */
export function planLoad(meta, st, loaded, accepted = 0) {
  const fresh = shardsFor(meta, st).filter((x) => !loaded.includes(x.code))
  const size = new Map((meta.shards ?? []).map((x) => [x.code, x.bytes ?? 0]))
  const sum = (codes) => codes.reduce((a, c) => a + (size.get(c) ?? 0), 0)
  const more = sum(fresh.map((x) => x.code))
  const bytes = sum(loaded) + more
  const step = (b) => Math.floor(b / WARN_BYTES)
  const warn = !!meta.shards && fresh.length > 0 && step(bytes) > step(accepted)
  return { load: warn ? [] : fresh, hold: warn ? fresh : [], bytes, more }
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
