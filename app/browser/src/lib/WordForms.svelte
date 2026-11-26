<script>
  // The word forms each term matched, as chips: unticking one excludes it.
  import { num } from './format.js'
  import { t as tr } from './i18n.js'

  let { forms, excl, ontoggle } = $props()
  const SHOW = 12
  let open = $state({}) // term key -> show all forms
</script>

<div class="forms">
  {#each forms as t (t.id)}
    <div class="term" role="group" aria-label={tr('wf.label', { t: t.key })}>
      <span class="key">{t.key}</span>
      {#if !t.forms.length}<span class="note">{tr('wf.none')}</span>{/if}
      {#each open[t.key] ? t.forms : t.forms.slice(0, SHOW) as f (f.form)}
        {@const on = !excl[t.key]?.includes(f.form)}
        <label class="chip" class:off={!on} title={tr('wf.title', { c: num(f.count), p: num(f.plans) })}>
          <input type="checkbox" checked={on} onchange={(e) => ontoggle(t.key, f.form, e.currentTarget.checked)} />
          {f.form} <small>{num(f.plans)}</small>
        </label>
      {/each}
      {#if t.forms.length > SHOW}
        <button class="link" aria-expanded={!!open[t.key]} onclick={() => (open[t.key] = !open[t.key])}>
          {open[t.key] ? tr('wf.fewer') : tr('wf.more', { n: t.forms.length - SHOW })}
        </button>
      {/if}
    </div>
  {/each}
</div>
