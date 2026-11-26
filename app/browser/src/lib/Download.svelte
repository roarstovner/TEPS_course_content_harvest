<script>
  // Download dialog: the selection the list shows, as one row per unit, in a
  // format; the worker builds the file (it holds the text).
  import { FORMATS, selection, UNITS } from '../download/table.js'
  import { EXCEL_MAX } from '../download/encode.js'
  import { num } from './format.js'

  let { open = $bindable(), model, view, s, query, excl, matches, meta, worker } = $props() // query: as run (in the header's section)

  let dialog = $state()
  let unit = $state('plan')
  let format = $state('csv')
  let count = $state.raw(null) // {rows, long} from the worker; null while counting
  let busy = $state(false)
  let error = $state(null)

  const instNames = $derived(Object.fromEntries((meta?.institutions ?? []).map((i) => [i.code, i.institution_name])))
  const sel = $derived(open && view ? selection(model, view, s, matches, instNames) : null)
  const q = $derived(matches ? query : '')
  const disabled = (u) => u === 'hit' && !q

  $effect(() => {
    if (!dialog) return
    if (open && !dialog.open) dialog.showModal()
    else if (!open && dialog.open) dialog.close()
  })
  $effect(() => {
    if (disabled(unit)) unit = 'plan'
    if (format === 'qda' && unit !== 'plan') format = 'csv'
  })

  const ctx = () => ({
    q,
    scope: s.scope,
    n: s.n,
    excl: $state.snapshot(excl),
    filters: { inst: [...s.inst], y0: s.y0 ?? model.yearFrom, y1: s.y1 ?? model.yearMax, st: [...s.st], subj: [...s.subj], lvl: [...s.lvl], cr: [...s.cr], lang: [...s.lang], sec: [...s.sec] },
    url: location.href,
    meta: { version: meta.version, frame: meta.frame, built_at: meta.built_at },
    date: new Date(),
  })
  const args = () => ({ unit, sel, ctx: ctx(), sectionOrder: meta.sections.map((s) => s.code) })

  // rows of the text units are counted in the worker
  let countId = 0
  $effect(() => {
    if (!sel) return
    const id = ++countId
    count = null
    error = null
    worker
      .call('downloadCount', args())
      .then((c) => id === countId && (count = c))
      .catch((e) => id === countId && (error = String(e?.message ?? e)))
  })

  async function download() {
    busy = true
    error = null
    try {
      const { name, blob } = await worker.call('download', { ...args(), format })
      const a = document.createElement('a')
      a.href = URL.createObjectURL(blob)
      a.download = name
      a.click()
      setTimeout(() => URL.revokeObjectURL(a.href), 10000)
    } catch (e) {
      error = String(e?.message ?? e)
    } finally {
      busy = false
    }
  }
</script>

<dialog bind:this={dialog} class="dl" onclose={() => (open = false)} aria-labelledby="dl-title">
  {#if sel}
    <h2 id="dl-title">Download the selection</h2>
    <p class="note">
      What the list shows: {num(sel.courses.length)} courses, {num(sel.plans.length)} plan versions{#if q}, matching
        <code>{q}</code>{/if}, with the filters{#if Object.keys(excl).length} and unticked word forms{/if}.
    </p>
    <div class="cols">
      <fieldset>
        <legend>One row per</legend>
        {#each Object.entries(UNITS) as [u, label] (u)}
          <label class="check"
            ><input type="radio" bind:group={unit} value={u} disabled={disabled(u)} /><span
              >{label}{#if u === 'hit' && !q}<small> (needs a query)</small>{/if}</span
            ></label
          >
        {/each}
      </fieldset>
      <fieldset>
        <legend>Format</legend>
        {#each Object.entries(FORMATS) as [f, label] (f)}
          <label class="check"
            ><input type="radio" bind:group={format} value={f} disabled={f === 'qda' && unit !== 'plan'} /><span>{label}</span></label
          >
        {/each}
      </fieldset>
    </div>
    <p class="count" aria-live="polite">
      {#if count}<strong>{num(count.rows)}</strong> rows{:else if !error}counting…{/if}
    </p>
    {#if format === 'xlsx' && (unit === 'plan' || unit === 'section')}
      <p class="note">
        Excel holds at most {num(EXCEL_MAX)} characters in a cell: longer texts are cut, and the column
        <code>truncated</code> flags the row{#if count}{' '}({num(count.long)} here){/if}. CSV and Parquet keep the whole text.
      </p>
    {/if}
    <p class="note">
      A ZIP with the {format === 'qda' ? 'text files and the classification sheet' : 'table'}, <code>manifest.json</code> (query,
      filters, data version, counts, the link to this view), <code>variables.csv</code>, the keys of the rows and
      <code>rebuild.R</code>/<code>rebuild.py</code>, which take the same rows from the published release.
    </p>
    {#if error}<p class="fail">{error}</p>{/if}
    <div class="buttons">
      <button type="button" onclick={() => (open = false)}>Cancel</button>
      <button type="button" class="primary" onclick={download} disabled={busy || !count?.rows}
        >{busy ? 'Building…' : 'Download'}</button
      >
    </div>
  {/if}
</dialog>

<style>
  .dl { border: 1px solid var(--line); border-radius: 8px; background: var(--bg); color: var(--fg); padding: 16px 20px; width: min(560px, calc(100vw - 32px)); }
  .dl::backdrop { background: rgb(0 0 0 / 0.35); }
  h2 { margin: 0 0 6px; font-size: 17px; }
  .cols { display: flex; gap: 24px; flex-wrap: wrap; margin-top: 10px; }
  .cols fieldset { flex: 1; min-width: 200px; }
  .note { font-size: 13px; margin: 6px 0; }
  .count { margin: 4px 0; }
  .buttons { display: flex; justify-content: flex-end; gap: 8px; margin-top: 14px; }
  .buttons button { border: 1px solid var(--line); background: var(--bg-2); border-radius: 4px; padding: 4px 14px; cursor: pointer; }
  .buttons .primary { background: var(--accent); border-color: var(--accent); color: var(--bg); }
  .buttons button:disabled { opacity: 0.5; cursor: default; }
</style>
