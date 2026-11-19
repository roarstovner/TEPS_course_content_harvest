<script>
  // Study type filter: DBH's study types grouped as in data/study_types.csv
  // (meta.study_types), each with the courses matching the query and the
  // other filters and the coverage of its course-years. A course-year belongs
  // to a type when DBH lists it under a programme of that type that year.
  // Types without courses in this data are greyed and folded in their group.
  import { untrack } from 'svelte'
  import { num, pct } from './format.js'

  let { types, selected = $bindable(), counts } = $props()

  let text = $state('')
  // open groups: those with a ticked type, else the first
  let open = $state(
    untrack(() => new Set(types.filter((t) => selected.includes(t.code)).map((t) => t.group).concat(selected.length ? [] : [types[0]?.group]))),
  )
  let openAbsent = $state(new Set())

  const groups = $derived.by(() => {
    const q = text.trim().toLowerCase()
    const by = new Map()
    for (const t of types) {
      if (q && ![t.code, t.name_en, t.name_no].some((s) => s?.toLowerCase().includes(q))) continue
      const g = by.get(t.group) ?? by.set(t.group, { code: t.group, label: t.group_label_en, title: t.group_label_no, items: [], absent: [] }).get(t.group)
      ;(t.courses ? g.items : g.absent).push(t)
    }
    return [...by.values()]
  })
  const searching = $derived(!!text.trim())
  const te = $derived(types.filter((t) => t.te_frame).map((t) => t.code))

  const toggle = (codes, on) => (selected = on ? [...new Set([...selected, ...codes])] : selected.filter((c) => !codes.includes(c)))
  const flip = (set, k) => {
    const s = new Set(set)
    s.has(k) ? s.delete(k) : s.add(k)
    return s
  }
  const ticked = (g) => g.items.filter((t) => selected.includes(t.code)).length
  const indeterminate = (n) => (el) => {
    el.indeterminate = n
  }
</script>

<details class="filter" open>
  <summary>Study type {#if selected.length}<span class="badge">{selected.length}</span>{/if}</summary>
  <div class="presets">
    <button class="link" onclick={() => (selected = [...te])} title={te.join(', ')}>Teacher education (TE frame)</button>
    {#if selected.length}<button class="link" onclick={() => (selected = [])}>Clear</button>{/if}
  </div>
  <input class="ffilter" type="search" placeholder="Filter {types.length} study types" aria-label="Filter study types" bind:value={text} />
  <ul class="tree">
    {#each groups as g (g.code)}
      {@const n = ticked(g)}
      {@const isOpen = searching || open.has(g.code)}
      <li>
        <div class="group">
          <button class="twist" aria-expanded={isOpen} aria-label="{isOpen ? 'Collapse' : 'Expand'} {g.label}" onclick={() => (open = flip(open, g.code))}>{isOpen ? '▾' : '▸'}</button>
          <label class="check" title={g.title}>
            <input
              type="checkbox"
              disabled={!g.items.length}
              checked={g.items.length > 0 && n === g.items.length}
              {@attach indeterminate(n > 0 && n < g.items.length)}
              onchange={(e) => toggle(g.items.map((t) => t.code), e.currentTarget.checked)}
            />
            <span>{g.label}</span>
            <small>{g.items.length}/{g.items.length + g.absent.length}</small>
          </label>
        </div>
        {#if isOpen}
          <ul>
            {#each g.items as t (t.code)}
              {@const c = counts?.get(t.code)}
              <li>
                <label class="check" title="{t.name_no} ({t.code})">
                  <input type="checkbox" checked={selected.includes(t.code)} onchange={(e) => toggle([t.code], e.currentTarget.checked)} />
                  <span>{t.name_en}</span>
                  <small
                    title="{num(c?.courses ?? 0)} courses match the query and the other filters; coverage: {num(c?.withPlan ?? 0)} of {num(
                      c?.courseYears ?? 0,
                    )} course-years have a plan">{num(c?.courses ?? 0)}<br />{pct(c?.withPlan ?? 0, c?.courseYears ?? 0)}</small
                  >
                </label>
              </li>
            {/each}
            {#if g.absent.length}
              {@const absOpen = searching || openAbsent.has(g.code)}
              <li>
                <button class="link absent-toggle" aria-expanded={absOpen} onclick={() => (openAbsent = flip(openAbsent, g.code))}
                  >{absOpen ? '▾' : '▸'} {g.absent.length} not in this data</button
                >
              </li>
              {#if absOpen}
                {#each g.absent as t (t.code)}
                  <li>
                    <label class="check absent" title="{t.name_no} ({t.code}): not in this data; the all-courses data adds it">
                      <input type="checkbox" disabled checked={selected.includes(t.code)} />
                      <span>{t.name_en}</span>
                    </label>
                  </li>
                {/each}
              {/if}
            {/if}
          </ul>
        {/if}
      </li>
    {:else}
      <li><small>No study type matches.</small></li>
    {/each}
  </ul>
</details>
