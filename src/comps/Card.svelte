<script>
    export let card
    export let type
</script>

<a href="/{type}/{card.text.replace(/\s+/g, '')}" class="card">
    <div class="bg" style="background-image: url('img/{card.img}');" />
    <div id="cont">
        <p>{card.text.toLowerCase()}</p>
    </div>
</a>

<style lang="scss">
    .card {
        cursor: pointer;
        aspect-ratio: 1;
        background-size: cover;
        position: relative;
        background-color: var(--pink);
        max-width: 34em;
        border: 0.3em solid var(--pink);
        #cont {
            position: absolute;
            bottom: 0;
            display: flex;
            align-items: center;
            padding: 0.3em 0em;
            width: 101%;
            transform: translateX(-0.1em);
            background-color: var(--pink);
            p {
                color: var(--white);
                padding-left: 0.28em;
                transform: translateY(0.08em);
            }
        }
        .bg {
            background-size: cover;
            height: 98%;
            transition: all 0.42s ease !important;
        }
    }
    .card:hover {
        .bg {
            filter: invert(100%);
        }
        #cont {
            p {
                color: var(--chad);
            }
        }
    }
</style>