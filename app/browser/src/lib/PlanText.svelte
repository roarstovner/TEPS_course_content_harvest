<script>
  // The full plan with the matches marked and, in the margin, the section
  // each line went to (README "Layout"); a legend switches the margin on/off.
  import { lineBlocks, lineSections } from '../data/overlay.js'
  import { segmentsIn } from './format.js'

  let { text, marks, sections, sectionsMeta } = $props()

  let overlay = $state(true)
  const colour = $derived(new Map(sectionsMeta.map((s, i) => [s.code, i])))
  const label = $derived(new Map(sectionsMeta.map((s) => [s.code, s.label_en])))
  const blocks = $derived(
    lineBlocks(lineSections(text, sections ? sectionsMeta.filter((s) => sections[s.code]).map((s) => [s.code, sections[s.code].text]) : [])),
  )
  const present = $derived(new Set(blocks.map((b) => b.kind)))
</script>

{#if sections}
  <div class="legend" aria-label="Sections in the margin">
    <label><input type="checkbox" bind:checked={overlay} /> sections in the margin</label>
    {#if overlay}
      {#each sectionsMeta as s (s.code)}
        <span class="key s{colour.get(s.code)}" class:absent={!present.has(s.code)} title={present.has(s.code) ? s.label_no : `${s.label_no}: not in this plan`}
          >{s.label_en}</span
        >
      {/each}
      <span class="key none">in no section</span>
    {/if}
  </div>
{:else}
  <p class="legend note">loading sections…</p>
{/if}
<div class="plantext lines" class:overlay={overlay && sections}>
  {#each blocks as b, i (i)}
    {@const sec = label.get(b.kind)}
    <!-- a block with a section can take focus, to show its label -->
    <!-- svelte-ignore a11y_no_noninteractive_tabindex -->
    <div
      class="blk {sec ? `s${colour.get(b.kind)}` : b.kind}"
      data-label={sec}
      title={sec}
      tabindex={sec && overlay && sections ? 0 : undefined}
    >{#each segmentsIn(text, b.start, b.end, marks) as g, j (j)}{#if g.term < 0}{g.text}{:else}<mark class="t{g.term % 6}">{g.text}</mark>{/if}{/each}{#if b.kind === 'blank'}&#8203;{/if}</div>
  {/each}
</div>
