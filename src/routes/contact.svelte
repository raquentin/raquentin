<script>
</script>

<div id="content">
    <div>
        <h1>contact</h1>
        <a href="/"><p>&larr; back to landing page</p></a>
    </div>
    <div id="contactBox">
        <a href="mailto:race@racewilliams.com"><h2>race@racewilliams.com</h2></a>
        <a href="mailto:race@gatech.edu"><h2>race@gatech.edu</h2></a>
        <a href="tel: 706-801-9509"><h2>(706) 801-9509</h2></a>
        <a href="https://discordapp.com/users/raise#2848"><h2>raise#2848</h2></a>
        <a href="https://www.instagram.com/racussy/"><h2>@racussy</h2></a>
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
        a {
            font-weight: bold;
        }
        a:hover {
            h2 {
                color: var(--pink);
            }
        }
        #contactBox {
            display: flex;
            align-items: center;
            justify-content: space-between;
            flex-direction: column;
            gap: 2em;
            transform: translateY(0.6em);
            margin-right: 1em;
        }
    }
</style>
