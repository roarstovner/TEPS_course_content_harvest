// The page's side of the worker: requests as promises, events to a callback.

export function startWorker(onEvent) {
  const w = new Worker(new URL('../worker.js', import.meta.url), { type: 'module' })
  const pending = new Map()
  let next = 0
  w.onmessage = ({ data }) => {
    if (data.id == null) return onEvent(data)
    const p = pending.get(data.id)
    pending.delete(data.id)
    if (data.error) p.reject(new Error(data.error))
    else p.resolve(data.result)
  }
  w.onerror = (e) => onEvent({ type: 'error', message: e.message || 'worker failed' })
  return {
    load: (base, version) => w.postMessage({ type: 'load', base, version }),
    call: (type, args) =>
      new Promise((resolve, reject) => {
        const id = next++
        pending.set(id, { resolve, reject })
        w.postMessage({ id, type, ...args })
      }),
  }
}
