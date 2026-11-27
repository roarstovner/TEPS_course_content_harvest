<script>
  // A collapsible filter of checkboxes, each with the courses matching the
  // query and the other filters (`counts`, value -> n), and a filter box for
  // long lists. Open at first when a value is ticked or `open` is set. Items
  // with a `group` (and `groupLabel`, `groupTitle`) are shown as a tree: a
  // group's box ticks all its items, its count is theirs summed (one value
  // per course); items without one follow the groups.
  import { untrack } from 'svelte'
  import { num } from './format.js'
  import { t } from './i18n.js'

  let { title, items, selected = $bindable(), counts, note = '', open = false } = $props()

  let isOpen = $state(untrack(() => open || selected.length > 0))
  let text = $state('')
  const shown = $derived(text ? items.filter((i) => i.label.toLowerCase().includes(text.trim().toLowerCase())) : items)
  const toggle = (vs, on) => (selected = on ? [...new Set([...selected, ...vs])] : selected.filter((x) => !vs.includes(x)))

  // the tree: groups in the order of their first item, then the loose items
  const tree = $derived.by(() => {
    if (!items.some((i) => i.group != null)) return null
    const by = new Map()
    const loose = []
    for (const i of shown) {
      if (i.group == null) loose.push(i)
      else (by.get(i.group) ?? by.set(i.group, { code: i.group, label: i.groupLabel ?? i.group, title: i.groupTitle, items: [] }).get(i.group)).items.push(i)
    }
    return { groups: [...by.values()], loose }
  })
  // open groups: those with a ticked item
  let openGroups = $state(untrack(() => new Set(items.filter((i) => i.group != null && selected.includes(i.value)).map((i) => i.group))))
  const flip = (k) => {
    const s = new Set(openGroups)
    s.has(k) ? s.delete(k) : s.add(k)
    openGroups = s
  }
  const indeterminate = (on) => (el) => {
    el.indeterminate = on
  }
</script>

{#snippet check(i)}
  {@const n = counts?.get(i.value) ?? 0}
  <label class="check" class:zero={!n} title={i.title ?? i.label}>
    <input type="checkbox" checked={selected.includes(i.value)} onchange={(e) => toggle([i.value], e.currentTarget.checked)} />
    <span>{i.label}</span>
    {#if counts}<small>{num(n)}</small>{/if}
  </label>
{/snippet}

<details class="filter" bind:open={isOpen}>
  <summary>
    {title}
    {#if selected.length}<span class="badge">{selected.length}</span>{/if}
    {#if note}<small class="fnote">{note}</small>{/if}
  </summary>
  {#if selected.length}<div class="presets"><button class="link" onclick={() => (selected = [])}>{t('f.clear')}</button></div>{/if}
  {#if items.length > 20}
    <input class="ffilter" type="search" placeholder={t('f.filterValues', { n: items.length })} aria-label={t('f.filterAria', { t: title })} bind:value={text} />
  {/if}
  <div class="flist" class:long={items.length > 20}>
    {#if tree}
      <ul class="tree">
        {#each tree.groups as g (g.code)}
          {@const vs = g.items.map((i) => i.value)}
          {@const k = vs.filter((v) => selected.includes(v)).length}
          {@const n = vs.reduce((s, v) => s + (counts?.get(v) ?? 0), 0)}
          {@const gOpen = !!text.trim() || openGroups.has(g.code)}
          <li>
            <div class="group">
              <button class="twist" aria-expanded={gOpen} aria-label={t(gOpen ? 'st.collapse' : 'st.expand', { g: g.label })} onclick={() => flip(g.code)}>{gOpen ? '▾' : '▸'}</button>
              <label class="check" class:zero={!n} title={g.title ?? g.label}>
                <input type="checkbox" checked={k === vs.length} {@attach indeterminate(k > 0 && k < vs.length)} onchange={(e) => toggle(vs, e.currentTarget.checked)} />
                <span>{g.label}</span>
                {#if counts}<small>{num(n)}</small>{/if}
              </label>
            </div>
            {#if gOpen}
              <ul>
                {#each g.items as i (i.value)}<li>{@render check(i)}</li>{/each}
              </ul>
            {/if}
          </li>
        {/each}
        {#each tree.loose as i (i.value)}<li>{@render check(i)}</li>{/each}
      </ul>
    {:else}
      {#each shown as i (i.value)}{@render check(i)}{/each}
    {/if}
  </div>
</details>
