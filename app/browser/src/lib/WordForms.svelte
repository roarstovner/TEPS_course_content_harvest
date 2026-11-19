<script>
  // The word forms each term matched, as chips: unticking one excludes it.
  import { num } from './format.js'

  let { forms, excl, ontoggle } = $props()
  const SHOW = 12
  let open = $state({}) // term key -> show all forms
</script>

<div class="forms">
  {#each forms as t (t.id)}
    <div class="term">
      <span class="key">{t.key}</span>
      {#if !t.forms.length}<span class="note">no match</span>{/if}
      {#each open[t.key] ? t.forms : t.forms.slice(0, SHOW) as f (f.form)}
        {@const on = !excl[t.key]?.includes(f.form)}
        <label class="chip" class:off={!on} title="{num(f.count)} matches in {num(f.plans)} plans">
          <input type="checkbox" checked={on} onchange={(e) => ontoggle(t.key, f.form, e.currentTarget.checked)} />
          {f.form} <small>{num(f.plans)}</small>
        </label>
      {/each}
      {#if t.forms.length > SHOW}
        <button class="link" onclick={() => (open[t.key] = !open[t.key])}>
          {open[t.key] ? 'fewer' : `+${t.forms.length - SHOW} more`}
        </button>
      {/if}
    </div>
  {/each}
</div>
