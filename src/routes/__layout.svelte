<script context="module">
    /** @type {import('@sveltejs/kit').Load} */
    export const load = async ({ url: { pathname } }) => ({
        props: { pathname },
    });
</script>

<script>
    import PageTrans from "../comps/PageTrans.svelte";
    export let pathname;
</script>

<main>
    <PageTrans {pathname}>
        <slot />
    </PageTrans>
</main>


<style lang="scss">
    :global(:root) {
        --pink: #F490B4;
        --purple: purple;
        --white: #ffffff;
        --grey: #242424;
    }
    main {
        background-color: var(--grey);
        min-height: 100vh;
        min-width: 100vw;
    }
</style>