<script>
  // Left pane: institution, years, study type.
  import StudyTypes from './StudyTypes.svelte'
  import { num } from './format.js'

  let { state = $bindable(), meta, model, instCounts } = $props()

  const years = $derived(Array.from({ length: model.yearMax - model.yearMin + 1 }, (_, i) => model.yearMin + i))

  function toggleInst(code, on) {
    state.inst = on ? [...state.inst, code] : state.inst.filter((c) => c !== code)
  }
  const setYear = (which, v) => {
    const y = Number(v)
    state[which] = y === (which === 'y0' ? model.yearMin : model.yearMax) ? null : y
  }
</script>

<fieldset>
  <legend>Years</legend>
  <div class="years">
    <select aria-label="From year" value={state.y0 ?? model.yearMin} onchange={(e) => setYear('y0', e.currentTarget.value)}>
      {#each years as y (y)}<option value={y} disabled={y > (state.y1 ?? model.yearMax)}>{y}</option>{/each}
    </select>
    –
    <select aria-label="To year" value={state.y1 ?? model.yearMax} onchange={(e) => setYear('y1', e.currentTarget.value)}>
      {#each years as y (y)}<option value={y} disabled={y < (state.y0 ?? model.yearMin)}>{y}</option>{/each}
    </select>
  </div>
</fieldset>

<StudyTypes types={meta.study_types} bind:selected={state.st} />

<fieldset>
  <legend>Institution {#if state.inst.length}<button class="link" onclick={() => (state.inst = [])}>all</button>{/if}</legend>
  {#each meta.institutions as i (i.code)}
    <label class="check" title={i.institution_name}>
      <input type="checkbox" checked={state.inst.includes(i.code)} onchange={(e) => toggleInst(i.code, e.currentTarget.checked)} />
      <span>{i.code.toUpperCase()}</span>
      <small>{num(instCounts.get(i.code) ?? 0)}</small>
    </label>
  {/each}
</fieldset>
