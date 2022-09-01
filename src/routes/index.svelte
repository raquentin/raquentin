<script>
    import ContentCenter from "../comps/ContentCenter.svelte"

    let greeting = "Hello!"
    const birthdate = new Date(2003, 10, 30, 1, 30, 0, 0);
    var now = new Date().getTime()
    let years = parseFloat(((now - birthdate) / 31556952000).toFixed(9))
    let count = setInterval(function() {
        var now = new Date().getTime()
        var distance = now - birthdate
        years = parseFloat((distance / 31556952000).toFixed(9))
        while (String(years).length < 12) {
            years = String(years) + "0"
        }
    }, 69)


    let buttonDirText = "to the right"
    function checkButtonDirText() {
        if (window.innerWidth < 1329) {
            buttonDirText = "below"
        } else {
            buttonDirText = "to the right"
        }
    }

</script>

<svelte:window on:resize={() => checkButtonDirText()} />


<ContentCenter>
    <div id="textBox">
        <h1>racewilliams.com</h1>
        <p>{greeting} I'm Race Williams, an <span style="color: var(--pink); font-size: 1em; margin-left: -0.08em">{years}</span> year-old student and software engineer working to progress the ability of computers to emulate human reasoning. Along the way, I'll continue to build open source applications, upload free resources to the rw.com library, and solve problems with software for employers. Learn more via the buttons {buttonDirText}.</p>
    </div>
    <div id="navBtnBox">
        <a class="navBtn" href="/apps"><h2 style="transform: translateY(-0.05em);">apps</h2></a>
        <a class="navBtn" href="/library"><h2 style="transform: translateY(0.01em);">library</h2></a>
        <a class="navBtn" href="/resume"><h2 style="transform: translateY(-0.02em);">resume</h2></a>
        <a class="navBtn" href="/contact"><h2 style="transform: translateY(-0.02em);">contact</h2></a>
    </div>
</ContentCenter>

<style lang="scss">
    h1 {
        color: var(--pink);
        margin-left: 0.2em;
    }
    p {
        font-weight: bold;
        max-width: 26em;
        margin-left: 0.1em;
        inline-size: 90vw;
    }
    #navBtnBox {
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-direction: column;
        gap: 2em;
        transform: translateY(0.6em);
        .navBtn {
            border: 0.4em solid var(--pink);
            border-radius: 0.8em;
            width: 14em;
            text-align: center;
            padding: 0.8em 0em;
            margin-right: 0.8em;
        }
        .navBtn:hover {
            background-color: var(--pink);
        }
    }

    #textBox {
        max-width: 100vw;
        display: flex;
        flex-direction: column;
        align-items: center;
    }

    @media all and (max-width: 1329px) {
        #navBtnBox {
            display: grid;
            grid-template-columns: 1fr 1fr;
            width: 100%;
            gap: 3em;
            transform: translateY(-2em);
            .navBtn {
                margin: auto auto;
            }
        }
        p {
            text-align: center;
            padding: 0.5em 0em;
        }
        h1 {
            margin-left: 0em;
        }
    }
    @media all and (max-width: 635px) {
        #navBtnBox {
            gap: 1em;
            .navBtn {
                width: 8em;
                margin-bottom: 1em;
                padding: 0.4em 0em;
            }
        }
    }
</style>
