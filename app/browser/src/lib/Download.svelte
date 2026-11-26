<script>
  // Download dialog: the selection the list shows, as one row per unit, in a
  // format; the worker builds the file (it holds the text).
  import { FORMATS, selection, UNITS } from '../download/table.js'
  import { EXCEL_MAX } from '../download/encode.js'
  import { num } from './format.js'
  import { t } from './i18n.js'

  let { open = $bindable(), model, view, s, query, excl, matches, meta, worker } = $props() // query: as run (in the header's section)

  let dialog = $state()
  const back = document.activeElement // the focus returns there on closing
  $effect(() => () => back instanceof HTMLElement && back.isConnected && back.focus())
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
    <h2 id="dl-title">{t('dl.title')}</h2>
    <p class="note">
      {t('dl.what', { c: num(sel.courses.length), p: num(sel.plans.length) })}{#if q}{t('dl.matching')}
        <code>{q}</code>{/if}{t('dl.filters')}{#if Object.keys(excl).length}{t('dl.excl')}{/if}.
    </p>
    <div class="cols">
      <fieldset>
        <legend>{t('dl.unit')}</legend>
        {#each Object.keys(UNITS) as u (u)}
          <label class="check"
            ><input type="radio" bind:group={unit} value={u} disabled={disabled(u)} /><span
              >{t(`unit.${u}`)}{#if u === 'hit' && !q}<small>{t('dl.needsQ')}</small>{/if}</span
            ></label
          >
        {/each}
      </fieldset>
      <fieldset>
        <legend>{t('dl.format')}</legend>
        {#each Object.entries(FORMATS) as [f, label] (f)}
          <label class="check"
            ><input type="radio" bind:group={format} value={f} disabled={f === 'qda' && unit !== 'plan'} /><span>{f === 'qda' ? t('fmt.qda') : label}</span></label
          >
        {/each}
      </fieldset>
    </div>
    <p class="count" aria-live="polite">
      {#if count}<strong>{num(count.rows)}</strong> {t('dl.rows')}{:else if !error}{t('dl.counting')}{/if}
    </p>
    {#if format === 'xlsx' && (unit === 'plan' || unit === 'section')}
      <p class="note">
        {t('dl.excel1', { n: num(EXCEL_MAX) })} <code>truncated</code> {t('dl.excel2')}{#if count}{' '}{t('dl.excelHere', { n: num(count.long) })}{/if}{t('dl.excel3')}
      </p>
    {/if}
    <p class="note">
      {format === 'qda' ? t('dl.zipQda') : t('dl.zipTable')} <code>manifest.json</code> {t('dl.zip2')} <code>variables.csv</code>{t('dl.zip3')}
      <code>rebuild.R</code>/<code>rebuild.py</code>{t('dl.zip4')}
    </p>
    {#if error}<p class="fail" role="alert">{error}</p>{/if}
    <div class="buttons">
      <button type="button" onclick={() => (open = false)}>{t('dl.cancel')}</button>
      <button type="button" class="primary" onclick={download} disabled={busy || !count?.rows}
        >{busy ? t('dl.building') : t('dl.download')}</button
      >
    </div>
  {/if}
</dialog>

<style>
  .cols { display: flex; gap: 24px; flex-wrap: wrap; margin-top: 10px; }
  .cols fieldset { flex: 1; min-width: 200px; }
  .count { margin: 4px 0; }
</style>
