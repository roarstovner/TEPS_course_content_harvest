<script>
  // A collapsible filter of checkboxes, each with the courses matching the
  // query and the other filters (`counts`, value -> n), and a filter box for
  // long lists. Open at first when a value is ticked or `open` is set.
  import { untrack } from 'svelte'
  import { num } from './format.js'

  let { title, items, selected = $bindable(), counts, note = '', open = false } = $props()

  let isOpen = $state(untrack(() => open || selected.length > 0))
  let text = $state('')
  const shown = $derived(text ? items.filter((i) => i.label.toLowerCase().includes(text.trim().toLowerCase())) : items)
  const toggle = (v, on) => (selected = on ? [...selected, v] : selected.filter((x) => x !== v))
</script>

<details class="filter" bind:open={isOpen}>
  <summary>
    {title}
    {#if selected.length}<span class="badge">{selected.length}</span>{/if}
    {#if note}<small class="fnote">{note}</small>{/if}
  </summary>
  {#if selected.length}<div class="presets"><button class="link" onclick={() => (selected = [])}>Clear</button></div>{/if}
  {#if items.length > 20}
    <input class="ffilter" type="search" placeholder="Filter {items.length} values" aria-label="Filter {title}" bind:value={text} />
  {/if}
  <div class="flist" class:long={items.length > 20}>
    {#each shown as i (i.value)}
      {@const n = counts?.get(i.value) ?? 0}
      <label class="check" class:zero={!n} title={i.title ?? i.label}>
        <input type="checkbox" checked={selected.includes(i.value)} onchange={(e) => toggle(i.value, e.currentTarget.checked)} />
        <span>{i.label}</span>
        {#if counts}<small>{num(n)}</small>{/if}
      </label>
    {/each}
  </div>
</details>
