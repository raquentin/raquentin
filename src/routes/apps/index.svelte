<script>
    import ContentTop from "../../comps/ContentTop.svelte"

import cardData from "./apps.json"
import Card from "../../comps/Card.svelte"
const cards = cardData.data
</script>

<ContentTop>
    <div>
        <h1>apps</h1>
        <a href="/"><p>&larr; back to landing page</p></a>
        <div id="grid">
            {#each cards as card}
                <Card {card}/>
            {/each}
        </div>
    </div>
</ContentTop>

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
    #grid {
        display: grid;
        margin-top: 1.5em;
        grid-template-columns: repeat(auto-fit, minmax(13em, 5fr));;
        gap: 2em;
        padding: 0 0.4em;
        padding-bottom: 10em;
    }
    
    @media all and (max-width: 800px) {
        #grid {
            gap: 1.4em;
            padding: 0 0.1em;
            grid-template-columns: repeat(auto-fit, minmax(10em, 5fr));;
        }
    }
</style>
