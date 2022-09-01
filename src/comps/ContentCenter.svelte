<script>

</script>

<div id="content">
    <slot/>
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
    }
</style>