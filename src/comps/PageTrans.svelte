<script>
    import { fly } from "svelte/transition";
    export let pathname = "";
</script>

<!-- Whenever pathname changes, unmount (page transition out), then re-mount (page transition in) -->
{#key pathname}
    <div
    in:fly={{ x: -5, duration: 250, delay: 250 }}
    out:fly={{ x: 5, duration: 250 }}
    >
        <slot />
    </div>
{/key}