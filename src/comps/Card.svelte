<script>
    export let card
    export let type
    let cardHref
    if (card.directLink != "") {
        cardHref = card.directLink
    } else {
        cardHref = `${type}/${card.text.replace(/\s+/g, '')}`
    }
</script>

<a href="{cardHref}" class="card">
    {#if card.directLink != ""}
    <div class="externalNotif"><p>this links to an external page</p></div>
    {/if}
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
        .externalNotif {
            position: absolute;
            opacity: 0;
            margin: 50% 50%;
            color: black !important;
            font-weight: bold;
            width: 100%;
            height: 40%;
            text-align: center;
            transform: translate(-50%, -50%);
            z-index: 12;
            p {
                width: 100%;
                height: 100%;
            }
        }
    }
    .card:hover {
        .bg {
            filter: invert(100%);
        }
        .externalNotif {
            opacity: 1;
            p {
                color: var(--black) !important;
            }
        }
    }
</style>