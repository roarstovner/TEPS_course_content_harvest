// Long scans in chunks, so that a newer request can supersede an older one:
// between chunks a scan yields to the event loop (where the worker takes the
// next message) and stops once its request is stale.

let channel = null
const waiting = []

/** Yield to the event loop: a macrotask, without the 4 ms clamp of nested
 *  timeouts. */
export function pause() {
  if (typeof MessageChannel === 'undefined') return new Promise((r) => setTimeout(r, 0))
  if (!channel) {
    channel = new MessageChannel()
    channel.port1.onmessage = () => {
      waiting.shift()()
      if (!waiting.length) channel.port1.unref?.() // node: the port keeps it alive only while used
    }
  }
  channel.port1.ref?.()
  return new Promise((r) => {
    waiting.push(r)
    channel.port2.postMessage(0)
  })
}

/**
 * A source of staleness tests: each call returns a test that turns true once
 * the source is called again (a newer request).
 * @returns {() => () => boolean}
 */
export function generations() {
  let g = 0
  return () => {
    const mine = ++g
    return () => mine !== g
  }
}

/**
 * Apply `fn` to `items` in chunks of `size`, yielding before each chunk but
 * the first; at least one chunk, so that no items give `fn([])`.
 * @param {() => boolean} stale checked before each chunk
 * @returns {Promise<any[]|null>} `fn`'s result per chunk, or null when stale
 */
export async function chunked(items, size, fn, stale, wait = pause) {
  const out = []
  for (let i = 0; i === 0 || i < items.length; i += size) {
    if (i) await wait()
    if (stale()) return null
    out.push(fn(items.slice(i, i + size)))
  }
  return out
}
