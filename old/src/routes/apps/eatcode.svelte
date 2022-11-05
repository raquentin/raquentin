<script>
    import ContentTop from "../../comps/ContentTop.svelte"
</script>

<ContentTop>
    <div>
        <div id="titleSplit">
            <div>
                <h1>eatcode</h1>
                <a href="/"><p>&larr; back to landing page</p></a>
            </div>
            <h2>what is Eatcode?</h2>
                <p>Eatcode is a technical interview training website for software engineers. I'm working on the webapp as Project Manager with a team of six other Georgia Tech CS students in association with GT Web Dev club. Thanks to its unique 3D food-themed UI, faster solution testing, and commitment to staying free, Eatcode surpasses its more popular competitors like Leetcode (🤮) and HackerRank (🤢). Eatcode is not yet a live app, but you can track our development <a href="https://github.com/eatcode-gt/eatcodeweb">on GitHub</a>.</p>
            <h2>gallery</h2>
                <div id="gallery">
                    <div class="column">
                        <div style='background-image: url("img/looper.jpg");' alt="3d pepper model">
                    </div>
            </div>
        </div>
    </div>
</ContentTop>

<style lang="scss">
    h1, h2 {
        color: var(--pink);
    }
    h2 {
        margin-top: 1.5em;
    }
    a {
        font-weight: bold;
        width: min-content;
        p {

        }
    }
    a:hover {
       color: var(--pink);
    }
    #gallery {
        display: flex;
        flex-wrap: wrap;
        gap: 1em;
    }
    .column {
        flex: 50%;
        max-width: 50%;
        img {
            vertical-align: middle;
            width: 100%;
        }
    }
</style>