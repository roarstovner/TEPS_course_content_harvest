<script>
  // Middle pane, help view: the user guide (GUIDE.md, the one source, also
  // read on GitHub). An example link (`?q=...`) runs its query in the app,
  // with the filters kept; a `#...` link scrolls to that heading.
  import guide from '../../GUIDE.md?raw'
  import { renderMarkdown } from './markdown.js'
  import { t } from './i18n.js'

  let { onexample } = $props()

  const { html, toc } = renderMarkdown(guide)
  let box = $state()

  const go = (id) => box?.querySelector(`#${CSS.escape(id)}`)?.scrollIntoView({ block: 'start' })

  function onclick(e) {
    const a = e.target instanceof Element ? e.target.closest('a') : null
    const h = a?.getAttribute('href')
    if (!h || e.ctrlKey || e.metaKey || e.shiftKey || e.button) return
    if (h.startsWith('?')) {
      e.preventDefault()
      onexample(h)
    } else if (h.startsWith('#')) {
      e.preventDefault()
      go(decodeURIComponent(h.slice(1)))
    }
  }
</script>

<div class="help" bind:this={box}>
  {#if t('help.enOnly')}<p class="note" lang="nb">{t('help.enOnly')}</p>{/if}
  <nav class="helptoc" aria-label={t('help.toc')}>
    {#each toc as t (t.id)}<button class="link" onclick={() => go(t.id)}>{t.text}</button>{/each}
  </nav>
  <!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_noninteractive_element_interactions (the links inside take the keys) -->
  <article class="guide" lang="en" {onclick}>{@html html}</article>
</div>
