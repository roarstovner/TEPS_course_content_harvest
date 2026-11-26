<script>
  // Middle pane, term lists: named lists of queries kept in this browser,
  // imported from and exported to YAML (the format of LK20_terms.yaml), run
  // in the worker as a heatmap term x institution x year, counted as the
  // trend view counts; a cell lists its courses; the matrix exports as CSV.
  import { inSection, parseQuery } from '../search/index.js'
  import { loadLists, matrixCsv, parseTermLists, saveLists, serializeTermList, slug, termMatrix } from '../data/termlists.js'
  import { share } from '../data/trend.js'
  import { num, pct } from './format.js'
  import { t as tr } from './i18n.js'

  let { model, s = $bindable(), meta, worker, plansReady, onpick } = $props()

  let lists = $state(loadLists())
  let stored = $state(true) // false when the browser does not keep them
  let error = $state(null)
  let fileInput = $state()
  const save = () => (stored = saveLists($state.snapshot(lists)))

  const names = $derived(Object.keys(lists).sort((a, b) => a.localeCompare(b)))
  const name = $derived(s.tl != null && lists[s.tl] ? s.tl : (names[0] ?? null))
  const list = $derived(name ? lists[name] : null)
  const terms = $derived(list?.terms ?? [])

  const unique = (base) => {
    let n = base
    for (let i = 2; lists[n]; i++) n = `${base} (${i})`
    return n
  }
  function newList() {
    const n = unique(tr('tl.newName'))
    lists[n] = { terms: [], meta: {} }
    s.tl = n
    save()
  }
  function rename(to) {
    to = to.trim()
    if (!to || to === name || !list) return
    to = unique(to)
    lists[to] = list
    delete lists[name]
    s.tl = to
    save()
  }
  function remove() {
    if (!list || !confirm(tr('tl.confirm', { n: name }))) return
    delete lists[name]
    s.tl = null
    save()
  }
  function addTerm(q = '') {
    if (!list) newList()
    lists[name].terms.push({ id: q ? slug(q) : `term${terms.length + 1}`, label: q, query: q })
    save()
  }
  function edit(i, field, value) {
    const t = lists[name].terms[i]
    t[field] = value.trim()
    if (field === 'label' && /^term\d+$/.test(t.id) && t.label) t.id = slug(t.label)
    save()
  }
  function drop(i) {
    lists[name].terms.splice(i, 1)
    save()
  }
  async function importFile(file) {
    error = null
    try {
      const got = await parseTermLists(await file.text(), file.name.replace(/\.ya?ml$/i, ''))
      let first = null
      for (const l of got) {
        const n = unique(l.name)
        lists[n] = { terms: l.terms, meta: l.meta }
        first ??= n
      }
      s.tl = first
      save()
    } catch (e) {
      error = `${file.name}: ${e?.message ?? e}`
    }
  }
  function saveFile(text, filename, type) {
    const a = document.createElement('a')
    a.href = URL.createObjectURL(new Blob([text], { type }))
    a.download = filename.replace(/[^\p{L}\p{N}._-]+/gu, '-')
    a.click()
    setTimeout(() => URL.revokeObjectURL(a.href), 10000)
  }
  async function exportYaml() {
    saveFile(await serializeTermList({ name, ...$state.snapshot(list) }), `${name}.yaml`, 'application/yaml')
  }
  function exportCsv() {
    const stamp = new Date().toISOString().slice(0, 19).replace(/[-:]/g, '')
    saveFile(matrixCsv(name, terms.map((t, k) => ({ ...t, query: queries[k] })), matrix, s.scope), `terms_${meta.version}_${name}_${stamp}Z.csv`, 'text/csv;charset=utf-8')
  }

  // the run: the plans each term matches, in the worker (filters apply here),
  // in the header's section when one is chosen (a term's own prefix wins)
  const queries = $derived(terms.map((t) => inSection(t.query, s.in)))
  const sig = $derived(JSON.stringify([queries, s.scope, s.scope === 'words' ? s.n : null]))
  let run = $state.raw(null) // {sig, results: [{keys: Set} | {error}], ms}
  let running = $state(false)
  $effect(() => {
    const [k, qs, scope, n] = [sig, queries, s.scope, s.n]
    if (!plansReady || !qs.length) return
    running = true
    const t0 = performance.now()
    worker.call('terms', { queries: $state.snapshot(qs), scope, n }).then(
      (r) => {
        if (r.superseded || k !== sig) return
        run = { sig: k, results: r.results.map((x) => (x.keys ? { keys: new Set(x.keys) } : x)), ms: performance.now() - t0 }
        running = false
      },
      (e) => ((error = String(e?.message ?? e)), (running = false)),
    )
  })
  const ready = $derived(run?.sig === sig)
  const matrix = $derived(ready ? termMatrix(model, s, run.results.map((r) => r.keys ?? null)) : null)

  // the heatmap's rows and the scale of its cells
  const rows = $derived(
    matrix
      ? terms.flatMap((t, k) => {
          const m = matrix.terms[k]
          if (!m) return [{ t, k, inst: null, error: run.results[k].error }]
          const all = { t, k, inst: null, points: m.all }
          return s.trows === 'inst' ? [all, ...matrix.institutions.map((i) => ({ t, k, inst: i, points: m.inst.get(i) }))] : [all]
        })
      : [],
  )
  const value = (p) => (s.tval === 'count' ? (p.withPlan ? p.matching : null) : share(p.matching, p.withPlan))
  const top = $derived(Math.max(0, ...rows.flatMap((r) => (r.points ?? []).map(value).filter((v) => v != null))))
  const shade = (v) => (top ? Math.round((100 * v) / top) : 0) // 0-100, scaled to --cell-max in the CSS
  // a share as a percentage: one decimal under 10%, so that a few plans do not read as 0
  const cellText = (v) => (s.tval === 'count' ? num(v) : v && v < 0.1 ? (100 * v).toFixed(1) : `${Math.round(100 * v)}`)
  const instName = $derived(new Map(meta.institutions.map((i) => [i.code, i.institution_name])))
</script>

<div class="trend terms">
  <div class="trendbar">
    <label
      >{tr('tl.list')} <select value={name} onchange={(e) => (s.tl = e.currentTarget.value)} disabled={!names.length}>
        {#each names as n (n)}<option value={n}>{n}</option>{/each}
      </select></label
    >
    {#if list}<input class="lname" value={name} aria-label={tr('tl.name')} onchange={(e) => rename(e.currentTarget.value)} />{/if}
    <button class="link" onclick={newList}>{tr('tl.new')}</button>
    <button class="link" onclick={() => fileInput.click()}>{tr('tl.import')}</button>
    <input type="file" accept=".yaml,.yml,text/yaml" hidden bind:this={fileInput} onchange={(e) => (e.currentTarget.files[0] && importFile(e.currentTarget.files[0]), (e.currentTarget.value = ''))} />
    {#if list}
      <button class="link" onclick={exportYaml}>{tr('tl.exportYaml')}</button>
      <button class="link" onclick={remove}>{tr('tl.delete')}</button>
    {/if}
  </div>
  <p class="note sub">
    {tr('tl.intro1')} <code>dybdelæring OR "deep learning"</code>{tr('tl.intro2')}{#if !stored}<strong class="fail">{tr('sc.notStored')}</strong
      >{/if}{tr('tl.intro3')} <code>LK20_terms.yaml</code> (README "Term Lists").
  </p>
  {#if error}<p class="fail" role="alert">{error}</p>{/if}

  {#if !list}
    <p class="hint">{tr('tl.none')}</p>
  {:else}
    {#if terms.length}
      <h3>{tr('tl.head')}</h3>
      <div class="trendbar">
        <span class="note">{tr('tl.rows')}</span>
        <label><input type="radio" bind:group={s.trows} value="all" /> {tr('tl.term')}</label>
        <label><input type="radio" bind:group={s.trows} value="inst" /> {tr('tl.termInst')}</label>
        <span class="note">{tr('tl.cells')}</span>
        <label><input type="radio" bind:group={s.tval} value="share" /> {tr('tl.share')}</label>
        <label><input type="radio" bind:group={s.tval} value="count" /> {tr('tl.cy')}</label>
        <span class="note">{#if running || !ready}{tr('tl.running', { n: terms.length })}{:else}{num(Math.round(run.ms))} ms{/if}</span>
        <button class="dlbutton" onclick={exportCsv} disabled={!matrix}>{tr('sc.export')}</button>
      </div>
      <p class="note sub">
        {tr('tl.heatIntro')} {#if s.tval === 'count'}{num(top)}{:else}{pct(top, 1)}{/if}. {tr('tl.click')}
      </p>
      {#if matrix}
        <div class="heatwrap">
          <table class="heat">
            <thead>
              <tr>
                <th>{tr('tl.termHead')}</th>
                {#if s.trows === 'inst'}<th>{tr('tl.inst')}</th>{/if}
                {#each matrix.years as y (y)}<th class="yr">{String(y).slice(2)}</th>{/each}
              </tr>
            </thead>
            <tbody>
              {#each rows as r (`${r.k}|${r.inst}`)}
                <tr class:first={!r.inst}>
                  <th title={r.t.query}>{r.inst ? '' : r.t.label || r.t.id}</th>
                  {#if s.trows === 'inst'}<td class="inst" title={instName.get(r.inst)}>{r.inst ? r.inst.toUpperCase() : tr('tl.all')}</td>{/if}
                  {#if r.error}
                    <td class="fail" colspan={matrix.years.length}>{r.error}</td>
                  {:else}
                    {#each r.points as p (p.year)}
                      {@const v = value(p)}
                      <td>
                        {#if v != null}
                          {@const tip = tr('tl.cell', { t: r.t.label || r.t.id, i: r.inst ? r.inst.toUpperCase() : tr('tl.all'), y: p.year, m: num(p.matching), w: num(p.withPlan), p: pct(p.matching, p.withPlan), n: num(p.n) })}
                          <button
                            class="cell"
                            style:--p={shade(v)}
                            title={tip}
                            aria-label={tip}
                            onclick={() => onpick({ q: r.t.query, inst: r.inst, y: p.year })}>{cellText(v)}</button
                          >
                        {/if}
                      </td>
                    {/each}
                  {/if}
                </tr>
              {/each}
            </tbody>
          </table>
        </div>
      {/if}
    {/if}
    <details class="ttable" open>
      <summary>{tr('tl.terms', { n: terms.length })}</summary>
      <table class="progs termtable">
        <thead><tr><th>{tr('tl.label')}</th><th>{tr('tl.query')}</th><th></th></tr></thead>
        <tbody>
          {#each terms as t, i (i)}
            {@const err = t.query ? parseQuery(t.query).error : null}
            <tr>
              <td><input value={t.label} placeholder={t.id} aria-label={tr('tl.label')} onchange={(e) => edit(i, 'label', e.currentTarget.value)} /></td>
              <td>
                <input class="q" value={t.query} aria-label={tr('tl.query')} aria-invalid={!!err} title={err?.message ?? ''} onchange={(e) => edit(i, 'query', e.currentTarget.value)} />
              </td>
              <td><button class="link" onclick={() => drop(i)} aria-label={tr('tl.removeAria', { t: t.label || t.id })}>{tr('tl.remove')}</button></td>
            </tr>
          {/each}
        </tbody>
      </table>
      <div class="trendbar">
        <button class="link" onclick={() => addTerm()}>{tr('tl.add')}</button>
        {#if s.q.trim()}<button class="link" onclick={() => addTerm(s.q.trim())}>{tr('tl.addQ')}</button>{/if}
      </div>
    </details>
  {/if}
</div>

<style>
  .lname { width: 14em; }
  .termtable { margin: 6px 0 4px; width: 100%; max-width: 70rem; }
  .termtable td:first-child { width: 14em; }
  .termtable input { width: 100%; font-size: 13px; }
  .termtable input.q { font-family: var(--mono); }
  .termtable input[aria-invalid='true'] { border-color: var(--error); }
  .heatwrap { overflow-x: auto; margin-top: 6px; }
  .heat { border-collapse: collapse; font-size: 12px; }
  .heat th { text-align: left; font-weight: 600; padding: 1px 8px 1px 0; white-space: nowrap; max-width: 16em; overflow: hidden; text-overflow: ellipsis; }
  .heat th.yr { text-align: center; color: var(--fg-2); font-weight: normal; padding: 0; }
  .heat td { padding: 1px; }
  .heat td.inst { color: var(--fg-2); padding-right: 8px; }
  .heat tr.first > * { border-top: 1px solid var(--line); }
  /* --p: 0-100 of the darkest cell, which is --cell-max of --cell-match (app.css) */
  .cell { background: color-mix(in srgb, var(--cell-match) calc(var(--p) * var(--cell-max) * 1%), transparent); width: 2.6em; height: 1.7em; border: 0; border-radius: 2px; font: 11px var(--font); font-variant-numeric: tabular-nums; cursor: pointer; padding: 0; outline: 1px solid var(--line); outline-offset: -1px; }
  .cell:hover { outline: 2px solid var(--accent); }
</style>
