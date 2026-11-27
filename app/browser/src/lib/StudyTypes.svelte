<script>
  // Study type filter: DBH's study types grouped as in data/study_types.csv
  // (meta.study_types), each with the courses matching the query and the
  // other filters and the coverage of its course-years. A course-year belongs
  // to a type when DBH lists it under a programme of that type that year.
  // Types without courses in this data are greyed and folded in their group.
  // Below the tree, per ticked type, the institutions with DBH courses of the
  // type that are not harvested (meta.uncovered): those with courses in the
  // current year first, former ones folded. In sharded data (all courses) a
  // type's courses are loaded when it is ticked: until then it shows the
  // courses of the whole data (meta) and the size of its shard.
  import { untrack } from 'svelte'
  import { mb, num, pct } from './format.js'
  import { local, other, t as tr } from './i18n.js'

  let { types, selected = $bindable(), counts, uncovered, shards = null, loaded = [] } = $props()
  const shardOf = $derived(new Map((shards ?? []).map((x) => [x.code, x])))

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
      const g = by.get(t.group) ?? by.set(t.group, { code: t.group, label: local(t, 'group_label'), title: other(t, 'group_label'), items: [], absent: [] }).get(t.group)
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
  // per ticked type with uncovered institutions: those with courses in
  // uncovered.year (current) and the former ones
  let openFormer = $state(new Set())
  const missing = $derived.by(() => {
    const rows = uncovered?.by_type ?? []
    return types
      .filter((t) => selected.includes(t.code))
      .map((t) => {
        const r = rows.filter((x) => x.study_type === t.code)
        return { type: t, current: r.filter((x) => x.courses_current > 0), former: r.filter((x) => !x.courses_current) }
      })
      .filter((m) => m.current.length || m.former.length)
  })
  const indeterminate = (n) => (el) => {
    el.indeterminate = n
  }
</script>

<details class="filter" open>
  <summary>{tr('st.title')} {#if selected.length}<span class="badge">{selected.length}</span>{/if}</summary>
  <div class="presets">
    <button class="link" onclick={() => (selected = [...te])} title={te.join(', ')}>{tr('st.te')}</button>
    {#if selected.length}<button class="link" onclick={() => (selected = [])}>{tr('f.clear')}</button>{/if}
  </div>
  <input class="ffilter" type="search" placeholder={tr('st.filter', { n: types.length })} aria-label={tr('st.filterAria')} bind:value={text} />
  <ul class="tree">
    {#each groups as g (g.code)}
      {@const n = ticked(g)}
      {@const isOpen = searching || open.has(g.code)}
      <li>
        <div class="group">
          <button class="twist" aria-expanded={isOpen} aria-label={tr(isOpen ? 'st.collapse' : 'st.expand', { g: g.label })} onclick={() => (open = flip(open, g.code))}>{isOpen ? '▾' : '▸'}</button>
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
              {@const sh = shardOf.get(t.code)}
              <li>
                <label class="check" title="{other(t, 'name')} ({t.code})">
                  <input type="checkbox" checked={selected.includes(t.code)} onchange={(e) => toggle([t.code], e.currentTarget.checked)} />
                  <span>{local(t, 'name')}</span>
                  {#if sh && !loaded.includes(t.code)}
                    <small class="unloaded" title={tr('st.unloaded', { c: num(t.courses), mb: mb(sh.bytes) })}>{num(t.courses)}<br />{mb(sh.bytes)}</small>
                  {:else}
                    <small title={tr('st.count', { c: num(c?.courses ?? 0), w: num(c?.withPlan ?? 0), n: num(c?.courseYears ?? 0) })}>{num(c?.courses ?? 0)}<br />{pct(c?.withPlan ?? 0, c?.courseYears ?? 0)}</small>
                  {/if}
                </label>
              </li>
            {/each}
            {#if g.absent.length}
              {@const absOpen = searching || openAbsent.has(g.code)}
              <li>
                <button class="link absent-toggle" aria-expanded={absOpen} onclick={() => (openAbsent = flip(openAbsent, g.code))}
                  >{absOpen ? '▾' : '▸'} {tr('st.absent', { n: g.absent.length })}</button
                >
              </li>
              {#if absOpen}
                {#each g.absent as t (t.code)}
                  <li>
                    <label class="check absent" title={tr('st.absentTitle', { name: other(t, 'name'), code: t.code })}>
                      <input type="checkbox" disabled checked={selected.includes(t.code)} />
                      <span>{local(t, 'name')}</span>
                    </label>
                  </li>
                {/each}
              {/if}
            {/if}
          </ul>
        {/if}
      </li>
    {:else}
      <li><small>{tr('st.none')}</small></li>
    {/each}
  </ul>
  {#if selected.length && uncovered}
    {@const y = uncovered.year}
    <div class="uncovered" role="note" aria-label={tr('st.uncAria')}>
      {#if missing.length}
        <p>{tr('st.unc', { y })}</p>
        <ul>
          {#each missing as m (m.type.code)}
            {@const fOpen = openFormer.has(m.type.code)}
            <li>
              <span title="{other(m.type, 'name')} ({m.type.code})">{local(m.type, 'name')}:</span>
              {#each m.current as r, i (r.institution_code)}{i ? '; ' : ' '}{tr('st.uncInst', { name: r.institution_name, n: num(r.courses_current), y })}{:else}{' '}{tr('st.uncNone', { y })}{/each}
              {#if m.former.length}
                <br /><button class="link" aria-expanded={fOpen} onclick={() => (openFormer = flip(openFormer, m.type.code))}
                  >{fOpen ? '▾' : '▸'} {tr('st.uncFormer', { n: m.former.length, y })}</button
                >
                {#if fOpen}
                  <ul>
                    {#each m.former as r (r.institution_code)}
                      <li>{tr('st.uncFormerInst', { name: r.institution_name, n: num(r.courses), last: r.year_last })}</li>
                    {/each}
                  </ul>
                {/if}
              {/if}
            </li>
          {/each}
        </ul>
      {:else}
        <p>{tr('st.uncAll')}</p>
      {/if}
    </div>
  {/if}
</details>
