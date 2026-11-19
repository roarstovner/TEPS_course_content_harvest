<script>
  // Study type filter: a plain checkbox list for now (a grouped tree will
  // replace this component; it keeps the same props).
  let { types, selected = $bindable() } = $props()

  function toggle(code, on) {
    selected = on ? [...selected, code] : selected.filter((c) => c !== code)
  }
</script>

<fieldset>
  <legend>Study type {#if selected.length}<button class="link" onclick={() => (selected = [])}>all</button>{/if}</legend>
  {#each types as t (t.code)}
    <label class="check" title={t.code}>
      <input type="checkbox" checked={selected.includes(t.code)} onchange={(e) => toggle(t.code, e.currentTarget.checked)} />
      <span>{t.name}</span>
    </label>
  {/each}
</fieldset>
