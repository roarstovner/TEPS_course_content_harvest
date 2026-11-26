<script>
  // Middle pane, screening view: a seeded random sample of the hits of the
  // selection (keyword in context, as the download unit `hit`), stepped
  // through with the keyboard and tagged include, exclude or maybe with a
  // note; the tags stay in this browser and export as CSV.
  import { selection } from '../download/table.js'
  import { hitId, loadTags, newSeed, saveTags, screeningCsv, storeKey, TAGS } from '../data/screening.js'
  import { num, pct } from './format.js'
  import { t as tr } from './i18n.js'

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
    if (t instanceof Element && t.closest('dialog')) return // a dialog keeps its keys
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
  <h3>{tr('sc.title')}</h3>
  {#if !q}
    <p class="note sub">{tr('sc.needQ')}</p>
  {:else}
    <div class="trendbar">
      <label>{tr('sc.sample')} <input type="number" class="n" min="1" max="100000" bind:value={s.sample} /> {tr('sc.hits')}</label>
      <label>{tr('sc.seed')} <input class="seed" bind:value={s.seed} spellcheck="false" /></label>
      <button class="link" onclick={() => (s.seed = newSeed())}>{tr('sc.new')}</button>
      <span class="note" role="status">{#if hits}{tr('sc.of', { a: num(hits.length), b: num(total) })}{:else if !error}{tr('sc.drawing')}{/if}</span>
      <button class="dlbutton" onclick={exportCsv} disabled={!hits?.length}>{tr('sc.export')}</button>
    </div>
    <p class="note sub">
      {tr('sc.note')}{#if !stored}<strong class="fail">{tr('sc.notStored')}</strong>{/if}. {tr('sc.keys')}
    </p>
    {#if error}<p class="fail" role="alert">{error}</p>{/if}
    {#if hits?.length}
      {@const tagged = tally.include + tally.exclude}
      <p class="tally">
        {tr('tag.include')} {num(tally.include)} · {tr('tag.exclude')} {num(tally.exclude)} · {tr('tag.maybe')} {num(tally.maybe)} · {tr('sc.untagged')}
        {num(hits.length - tagged - tally.maybe)}{#if tagged}{' · '}{tr('sc.precision')} {pct(tally.include, tagged)}
          <span class="note">{tr('sc.precisionNote')}</span>{/if}
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
              <button class={g} aria-pressed={mine.tag === g} aria-keyshortcuts={k} onclick={() => tag(g)}>{tr(`tag.${g}`)} <kbd>{k}</kbd></button>
            {/each}
            <button class="link" aria-keyshortcuts="o" onclick={() => onopen(hit)}>{tr('sc.open')}</button>
          </div>
          <textarea
            bind:this={noteBox}
            rows="2"
            placeholder={tr('sc.notePh')}
            aria-label={tr('sc.notePh')}
            value={mine.note ?? ''}
            onchange={(e) => set(hit, { note: e.currentTarget.value })}
          ></textarea>
        </article>
      {/if}
      <ol class="hitlist" bind:this={list} aria-label={tr('sc.list')}>
        {#each hits as h, i (hitId(h))}
          {@const g = tags[hitId(h)]?.tag}
          <li data-i={i} class:current={i === cur}>
            <button class="row2" aria-current={i === cur ? 'true' : undefined} onclick={() => (cur = i)}>
              <span class="tag {g ?? ''}" aria-hidden="true" title={g ? tr(`tag.${g}`) : undefined}>{g ? TAGS[g].toUpperCase() : '·'}</span>{#if g}<span class="sr">{tr(`tag.${g}`)}</span>{/if}
              <span class="where">{h.institution.toUpperCase()} {h.course_code}</span>
              <span class="kw">{h.before.slice(-40)}<mark>{h.match}</mark>{h.after.slice(0, 40)}</span>
            </button>
          </li>
        {/each}
      </ol>
    {:else if hits}
      <p class="note sub">{tr('sc.noHits')}</p>
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
  .tags button:not(.link) { border: 1px solid var(--ctl); background: var(--bg-2); border-radius: 4px; padding: 2px 10px; cursor: pointer; }
  .tags button[aria-pressed='true'] { background: var(--accent); border-color: var(--accent); color: var(--bg); }
  .tags button[aria-pressed='true'] kbd { border-color: var(--bg); }
  textarea { display: block; width: 100%; margin-top: 8px; font: inherit; color: inherit; background: var(--bg); border: 1px solid var(--ctl); border-radius: 4px; padding: 4px 6px; }
  .hitlist { list-style: none; margin: 0; padding: 0; font-size: 12px; }
  .hitlist li.current { background: var(--accent-bg); }
  .row2 { display: grid; grid-template-columns: 1.5em 9em 1fr; gap: 8px; width: 100%; text-align: left; border: 0; border-bottom: 1px solid var(--line); background: none; padding: 2px 4px; cursor: pointer; }
  .kw { overflow: hidden; white-space: nowrap; text-overflow: ellipsis; }
  .tag { text-align: center; font-weight: 600; color: var(--fg-2); }
  .tag.include { color: var(--tag-include); }
  .tag.exclude { color: var(--tag-exclude); }
  .tag.maybe { color: var(--tag-maybe); }
</style>
