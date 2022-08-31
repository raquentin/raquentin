<script>
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
</script>

<div id="content">
    <div>
        <h1>racewilliams.com</h1>
        <p>{greeting} I'm Race Williams, an <span style="color: var(--pink); font-size: 1em; margin-left: -0.08em">{years}</span> year-old student and software engineer working to progress the ability of computers to emulate human reasoning. Along the way, I'll continue to build open source applications, upload free resources to the rw.com library, and solve problems with software for employers. Learn more via the buttons to the right.</p>
    </div>
    <div id="navBtnBox">
        <a class="navBtn" href="/apps"><h2 style="transform: translateY(-0.05em);">apps</h2></a>
        <a class="navBtn" href="/library"><h2 style="transform: translateY(0.01em);">library</h2></a>
        <a class="navBtn" href="/resume"><h2 style="transform: translateY(-0.02em);">resume</h2></a>
    </div>
</div>

<style lang="scss">
    #content {
        display: flex;
        align-items: center;
        justify-content: space-between;
        height: 100vh;
        max-height: 100vh;
        padding: 0em 12em;
        transform: translateY(-3em);
        h1 {
            color: var(--pink);
        }
        p {
            font-weight: bold;
            max-width: 26em;
            margin-left: 0.1em;
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
                width: 16em;
                text-align: center;
                padding: 0.8em 0em;
                margin-right: 0.8em;
            }
            .navBtn:hover {
                background-color: var(--pink);
            }
        }
    }
</style>
