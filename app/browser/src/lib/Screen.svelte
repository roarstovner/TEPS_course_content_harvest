<script>
  // Middle pane, screening view: a seeded random sample of the hits of the
  // selection (keyword in context, as the download unit `hit`), stepped
  // through with the keyboard and tagged include, exclude or maybe with a
  // note; the tags stay in this browser and export as CSV.
  import { selection } from '../download/table.js'
  import { hitId, loadTags, newSeed, saveTags, screeningCsv, storeKey, TAGS } from '../data/screening.js'
  import { num, pct } from './format.js'

  let { model, view, s = $bindable(), q, excl, matches, meta, worker, onopen } = $props()

  let hits = $state.raw(null) // the sample; null while drawing
  let total = $state(0)
  let error = $state(null)
  let cur = $state(0)
  let tags = $state({})
  let stored = $state(true) // false when the browser does not keep them
  let noteBox = $state()
  let list = $state()

  // a seed is drawn on entering the view, so the sample can be cited
  $effect(() => {
    if (!s.seed) s.seed = newSeed()
  })

  const key = $derived(q ? storeKey(meta.frame, { q, scope: s.scope, n: s.n, excl }) : null)
  $effect(() => {
    tags = key ? loadTags(key) : {}
  })

  const instNames = $derived(Object.fromEntries((meta?.institutions ?? []).map((i) => [i.code, i.institution_name])))
  const plans = $derived(q && view ? selection(model, view, s, matches, instNames).plans : null)
  let reqId = 0
  $effect(() => {
    const [size, seed] = [s.sample, s.seed]
    if (!plans || !seed || !Number.isInteger(size) || size < 1) return
    const id = ++reqId
    const ctx = { q, scope: s.scope, n: s.n, excl: $state.snapshot(excl) }
    hits = null
    error = null
    worker.call('sample', { sel: { plans }, ctx, size, seed }).then(
      (r) => {
        if (id !== reqId) return
        total = r.total
        hits = r.hits
        // start at the first hit not yet tagged
        const i = r.hits.findIndex((h) => !tags[hitId(h)]?.tag)
        cur = i < 0 ? 0 : i
      },
      (e) => id === reqId && (error = String(e?.message ?? e)),
    )
  })

  const hit = $derived(hits?.[cur] ?? null)
  const mine = $derived(hit ? (tags[hitId(hit)] ?? {}) : {})
  const tally = $derived.by(() => {
    const t = { include: 0, exclude: 0, maybe: 0 }
    for (const h of hits ?? []) {
      const g = tags[hitId(h)]?.tag
      if (g) t[g]++
    }
    return t
  })

  function set(h, change) {
    const id = hitId(h)
    const next = { ...(tags[id] ?? {}), ...change }
    const t = { ...tags }
    if (next.tag || next.note) t[id] = next
    else delete t[id]
    tags = t
    stored = saveTags(key, $state.snapshot(t))
  }
  function tag(g) {
    if (!hit) return
    const next = mine.tag === g ? null : g // the same key again clears it
    set(hit, { tag: next })
    if (next && cur < hits.length - 1) cur++
  }
  const go = (i) => hits?.length && (cur = Math.max(0, Math.min(hits.length - 1, i)))
  $effect(() => {
    list?.querySelector(`[data-i="${cur}"]`)?.scrollIntoView({ block: 'nearest' })
  })

  function onKey(e) {
    const t = e.target
    if (t instanceof HTMLElement && (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName))) return
    if (e.ctrlKey || e.metaKey || e.altKey || !hits?.length) return
    const g = Object.keys(TAGS).find((k) => TAGS[k] === e.key)
    if (e.key === 'j' || e.key === 'ArrowDown' || e.key === 'ArrowRight') go(cur + 1)
    else if (e.key === 'k' || e.key === 'ArrowUp' || e.key === 'ArrowLeft') go(cur - 1)
    else if (g) tag(g)
    else if (e.key === 'n') noteBox?.focus()
    else if (e.key === 'o' && hit) onopen(hit)
    else return
    e.preventDefault()
  }

  function exportCsv() {
    const csv = screeningCsv(hits, tags, { q, seed: s.seed })
    const stamp = new Date().toISOString().slice(0, 19).replace(/[-:]/g, '')
    const a = document.createElement('a')
    a.href = URL.createObjectURL(new Blob([csv], { type: 'text/csv;charset=utf-8' }))
    a.download = `screening_${meta.version}_seed${s.seed}_${stamp}Z.csv`.replace(/[^\w.-]+/g, '-')
    a.click()
    setTimeout(() => URL.revokeObjectURL(a.href), 10000)
  }
</script>

<svelte:window onkeydown={onKey} />

<div class="trend screen">
  <h3>Screening</h3>
  {#if !q}
    <p class="note sub">Screening needs a query: it draws a random sample of its hits (keyword in context).</p>
  {:else}
    <div class="trendbar">
      <label>Sample <input type="number" class="n" min="1" max="100000" bind:value={s.sample} /> hits</label>
      <label>seed <input class="seed" bind:value={s.seed} spellcheck="false" /></label>
      <button class="link" onclick={() => (s.seed = newSeed())}>new sample</button>
      <span class="note">{#if hits}{num(hits.length)} of {num(total)} hits{:else if !error}drawing…{/if}</span>
      <button class="dlbutton" onclick={exportCsv} disabled={!hits?.length}>Export CSV</button>
    </div>
    <p class="note sub">
      The sample and the seed are in the link. Tags are kept in this browser for this query and data frame{#if !stored}<strong class="fail">
          — this browser does not store them: export before leaving</strong
        >{/if}. Keys: <kbd>j</kbd>/<kbd>k</kbd> next, previous, <kbd>i</kbd> include, <kbd>e</kbd> exclude, <kbd>m</kbd> maybe,
      <kbd>n</kbd> note, <kbd>o</kbd> open the plan.
    </p>
    {#if error}<p class="fail">{error}</p>{/if}
    {#if hits?.length}
      {@const tagged = tally.include + tally.exclude}
      <p class="tally">
        Include {num(tally.include)} · exclude {num(tally.exclude)} · maybe {num(tally.maybe)} · untagged
        {num(hits.length - tagged - tally.maybe)}{#if tagged}{' · '}precision {pct(tally.include, tagged)}
          <span class="note">(include of include + exclude)</span>{/if}
      </p>
      {#if hit}
        <article class="card {mine.tag ?? ''}">
          <header>
            <strong>{cur + 1}</strong> / {num(hits.length)} · {hit.institution.toUpperCase()}
            {hit.course_code} {hit.course_name} · {hit.year_from}–{hit.year_to}{#if hit.section} · {hit.section}{/if}
            · <code>{hit.term}</code>
          </header>
          <p class="ctx">…{hit.before}<mark>{hit.match}</mark>{hit.after}…</p>
          <div class="tags">
            {#each Object.entries(TAGS) as [g, k] (g)}
              <button class={g} aria-pressed={mine.tag === g} onclick={() => tag(g)}>{g} <kbd>{k}</kbd></button>
            {/each}
            <button class="link" onclick={() => onopen(hit)}>open the plan</button>
          </div>
          <textarea
            bind:this={noteBox}
            rows="2"
            placeholder="Note"
            value={mine.note ?? ''}
            onchange={(e) => set(hit, { note: e.currentTarget.value })}
          ></textarea>
        </article>
      {/if}
      <ol class="hitlist" bind:this={list}>
        {#each hits as h, i (hitId(h))}
          {@const g = tags[hitId(h)]?.tag}
          <li data-i={i} class:current={i === cur}>
            <button class="row2" onclick={() => (cur = i)}>
              <span class="tag {g ?? ''}">{g ? g[0].toUpperCase() : '·'}</span>
              <span class="where">{h.institution.toUpperCase()} {h.course_code}</span>
              <span class="kw">{h.before.slice(-40)}<mark>{h.match}</mark>{h.after.slice(0, 40)}</span>
            </button>
          </li>
        {/each}
      </ol>
    {:else if hits}
      <p class="note sub">No hits in the selection.</p>
    {/if}
  {/if}
</div>

<style>
  .seed { width: 7em; font-family: var(--mono); }
  .tally { font-size: 13px; margin: 4px 0 8px; }
  .card { border: 1px solid var(--line); border-left: 4px solid var(--line); border-radius: 4px; padding: 8px 12px; margin-bottom: 10px; max-width: 80ch; }
  .card.include { border-left-color: var(--c3); }
  .card.exclude { border-left-color: var(--c8); }
  .card.maybe { border-left-color: var(--c4); }
  .card header { font-size: 13px; color: var(--fg-2); }
  .ctx { font-size: 15px; margin: 8px 0; white-space: pre-wrap; overflow-wrap: anywhere; }
  .tags { display: flex; gap: 8px; align-items: center; flex-wrap: wrap; }
  .tags button:not(.link) { border: 1px solid var(--line); background: var(--bg-2); border-radius: 4px; padding: 2px 10px; cursor: pointer; }
  .tags button[aria-pressed='true'] { background: var(--accent); border-color: var(--accent); color: var(--bg); }
  .tags button[aria-pressed='true'] kbd { border-color: var(--bg); }
  textarea { display: block; width: 100%; margin-top: 8px; font: inherit; color: inherit; background: var(--bg); border: 1px solid var(--line); border-radius: 4px; padding: 4px 6px; }
  .hitlist { list-style: none; margin: 0; padding: 0; font-size: 12px; }
  .hitlist li.current { background: var(--accent-bg); }
  .row2 { display: grid; grid-template-columns: 1.5em 9em 1fr; gap: 8px; width: 100%; text-align: left; border: 0; border-bottom: 1px solid var(--line); background: none; padding: 2px 4px; cursor: pointer; }
  .kw { overflow: hidden; white-space: nowrap; text-overflow: ellipsis; }
  .tag { text-align: center; font-weight: 600; color: var(--fg-2); }
  .tag.include { color: var(--c3); }
  .tag.exclude { color: var(--c8); }
  .tag.maybe { color: var(--c4); }
</style>
