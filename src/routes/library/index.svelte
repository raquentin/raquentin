<script>
    import cardData from "./library.json"
    const cards = cardData.data
    import Card from "../../comps/Card.svelte"
</script>

<div id="content">
    <div>
        <h1>library</h1>
        <a href="/"><p>&larr; back to landing page</p></a>
        <div id="grid">
            {#each cards as card}
                <Card {card}/>
            {/each}
        </div>
    </div>
</div>

<style lang="scss">
    #content {
        height: 100vh;
        max-height: 100vh;
        padding: 6em 12em;
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
        #grid {
            display: grid;
            margin-top: 1.5em;
            grid-template-columns: repeat(auto-fit, minmax(13em, 5fr));;
            gap: 2em;
            padding: 0 0.4em;
            padding-bottom: 10em;
        }
    }
</style>
