<script>

</script>

<div id="content">
    <slot/>
</div>

<style lang="scss">
    #content {
        height: 100vh;
        max-height: 100vh;
        padding: 6em 12em;
    }
    @media all and (max-width: 1430px) {
        #content {
            padding: 6em 8em;
        }
    }
    @media all and (max-width: 864px) {
        #content {
            padding: 6em 5em;
        }
    }
</style>