<script context="module">
    export function load({ error, status }) {
        return {
            props: {
                title: `${status}: ${error.message}`
            }
        };
    }
</script>

<script>
    import ContentCenter from "../comps/ContentCenter.svelte"
    export let title
</script>

<ContentCenter>
    <div>
        <h1>oops! 404</h1>
        <a href="/"><p>&larr; back to landing page</p></a>
        <br/>
        <p>{title}</p>
    </div>
</ContentCenter>

<style lang="scss">
    h1 {
        color: var(--pink);
    }
    a {
        font-weight: bold;
    }
    a:hover {
        p {
            color: var(--pink);
        }
    }
</style>
