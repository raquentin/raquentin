<script context="module">
    /** @type {import('@sveltejs/kit').Load} */
    export const load = async ({ url: { pathname } }) => ({
        props: { pathname },
    });
</script>

<script>
    import PageTrans from "../comps/PageTrans.svelte";
    export let pathname;

    import { afterNavigate } from '$app/navigation';
    import { fly } from 'svelte/transition';

    // hide by default
    let visible = false;

    let duration;

    afterNavigate(({ from }) => {
        // only animate if the navigation came from outside the page
        duration = from === null ? 200 : 0;
        // toggle visbility in any case
        visible = true;
    });
</script>

{#if visible}
<main in:fly={{ x: -20, duration: duration, delay: 200 }}>
    <PageTrans {pathname}>
        <slot />
    </PageTrans>
</main>
{/if}

<style lang="scss">
    :global(:root) {
        --pink: #F490B4;
        --white: #ffffff;
        --grey: #242424;
    }
    main {
        background-color: var(--grey);
        min-height: 100vh;
        min-width: 100vw;
    }
</style>