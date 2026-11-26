<script>
  // The full plan with the matches marked and, in the margin, the section
  // each line went to (README "Layout"); a legend switches the margin on/off
  // (remembered in localStorage). The legend stays above the text's own
  // scroller, so it never covers a line.
  import { lineBlocks, lineSections } from '../data/overlay.js'
  import { segmentsIn } from './format.js'
  import { local, other, t } from './i18n.js'

  let { text, marks, sections, sectionsMeta } = $props()

  const KEY = 'plan-overlay'
  let overlay = $state(load())
  function load() {
    try {
      return localStorage.getItem(KEY) !== 'off'
    } catch {
      return true
    }
  }
  $effect(() => {
    try {
      localStorage.setItem(KEY, overlay ? 'on' : 'off')
    } catch {
      // storage blocked: the toggle lasts until the page is left
    }
  })
  const colour = $derived(new Map(sectionsMeta.map((s, i) => [s.code, i])))
  const label = $derived(new Map(sectionsMeta.map((s) => [s.code, local(s, 'label')])))
  const blocks = $derived(
    lineBlocks(lineSections(text, sections ? sectionsMeta.filter((s) => sections[s.code]).map((s) => [s.code, sections[s.code].text]) : [])),
  )
  const present = $derived(new Set(blocks.map((b) => b.kind)))
</script>

{#if sections}
  <div class="legend" role="group" aria-label={t('pt.legend')}>
    <label><input type="checkbox" bind:checked={overlay} /> {t('pt.toggle')}</label>
    {#if overlay}
      {#each sectionsMeta as s (s.code)}
        <span class="key s{colour.get(s.code)}" class:absent={!present.has(s.code)} title={present.has(s.code) ? other(s, 'label') : t('pt.notIn', { s: other(s, 'label') })}
          >{local(s, 'label')}</span
        >
      {/each}
      <span class="key none">{t('pt.none')}</span>
    {/if}
  </div>
{:else}
  <p class="legend note" role="status">{t('c.sections')}</p>
{/if}
<div class="ptext">
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
</div>
