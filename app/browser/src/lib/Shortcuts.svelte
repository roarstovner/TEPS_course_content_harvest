<script>
  // The keyboard shortcuts (`?`), a modal dialog in the UI language: Esc or
  // Close shuts it and the focus goes back to where it was.
  import { SHORTCUTS, t } from './i18n.js'

  let { open = $bindable(), onguide } = $props()

  let dialog = $state()
  const back = document.activeElement

  $effect(() => {
    dialog.showModal()
    return () => back instanceof HTMLElement && back.isConnected && back.focus()
  })
  function guide() {
    open = false
    onguide()
  }
</script>

<dialog bind:this={dialog} class="dl keys" onclose={() => (open = false)} aria-labelledby="keys-title">
  <h2 id="keys-title">{t('key.title')}</h2>
  {#each [['general', 'key.general'], ['screen', 'key.screenHead']] as [group, head] (group)}
    <h3>{t(head)}</h3>
    <table>
      <tbody>
        {#each SHORTCUTS[group] as [keys, desc] (desc)}
          <tr>
            <th scope="row">{#each keys as k, i (k)}{i ? ' / ' : ''}<kbd>{k}</kbd>{/each}</th>
            <td>{t(desc)}</td>
          </tr>
        {/each}
      </tbody>
    </table>
  {/each}
  <p class="note">{t('key.typing')}</p>
  <div class="buttons">
    <button type="button" onclick={guide}>{t('key.guide')}</button>
    <button type="button" class="primary" onclick={() => (open = false)}>{t('key.close')}</button>
  </div>
</dialog>

<style>
  h3 { font-size: 13px; margin: 12px 0 4px; color: var(--fg-2); }
  table { border-collapse: collapse; font-size: 13px; }
  th, td { text-align: left; padding: 3px 14px 3px 0; vertical-align: baseline; border-bottom: 1px solid var(--line); }
  th { font-weight: normal; white-space: nowrap; }
</style>
