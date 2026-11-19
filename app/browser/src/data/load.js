// Fetching the data files, with the Cache API keyed on the data version, and
// reading Parquet. Works in the page and in the worker; every cache access is
// guarded, so the app works where the Cache API is missing or blocked.

import { parquetMetadata, parquetReadObjects, parquetSchema } from 'hyparquet'
import { compressors } from 'hyparquet-compressors'

const CACHE = 'teps-course-browser'

async function readBody(res, onProgress) {
  const total = Number(res.headers.get('content-length')) || 0
  if (!res.body || !onProgress) return res.arrayBuffer()
  const reader = res.body.getReader()
  const chunks = []
  let loaded = 0
  for (;;) {
    const { done, value } = await reader.read()
    if (done) break
    chunks.push(value)
    loaded += value.length
    onProgress(loaded, Math.max(total, loaded))
  }
  const out = new Uint8Array(loaded)
  let off = 0
  for (const c of chunks) {
    out.set(c, off)
    off += c.length
  }
  return out.buffer
}

/**
 * Fetch `url` (absolute) as an ArrayBuffer, from the cache when it holds the
 * file for `version`; a fetched file replaces older versions in the cache.
 * @param {(loaded: number, total: number) => void} [onProgress]
 */
export async function fetchCached(url, version, onProgress) {
  const key = `${url}?v=${encodeURIComponent(version)}`
  let cache = null
  try {
    cache = await caches.open(CACHE)
    const hit = await cache.match(key)
    if (hit) {
      const buf = await hit.arrayBuffer()
      onProgress?.(buf.byteLength, buf.byteLength)
      return buf
    }
  } catch {
    cache = null
  }
  const res = await fetch(url, { cache: 'no-cache' })
  if (!res.ok) throw new Error(`${url}: HTTP ${res.status}`)
  const buf = await readBody(res, onProgress)
  if (cache) {
    try {
      await cache.put(key, new Response(buf))
      for (const req of await cache.keys()) if (req.url.startsWith(`${url}?v=`) && req.url !== key) await cache.delete(req)
    } catch {
      // a full or blocked cache only costs the next visit a download
    }
  }
  return buf
}

/** Fetch meta.json, always revalidated: its `version` keys the cache. */
export async function fetchMeta(base) {
  const res = await fetch(new URL('meta.json', base), { cache: 'no-cache' })
  if (!res.ok) throw new Error(`meta.json: HTTP ${res.status}`)
  return res.json()
}

/** Rows of a Parquet file as objects; `opts` as in hyparquet: `rowStart`,
 *  `rowEnd`, `parsers` (e.g. `stringFromBytes`, to turn each text into
 *  something else as it is read). */
export function readParquet(buf, columns, opts = {}) {
  return parquetReadObjects({ file: buf, compressors, columns, ...opts })
}

/** The column names and the number of rows of a Parquet file. */
export function parquetInfo(buf) {
  const md = parquetMetadata(buf)
  return { columns: parquetSchema(md).children.map((c) => c.element.name), rows: Number(md.num_rows) }
}
