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
    
    @media all and (max-width: 1700px) {
        #content {
            padding: 0em 10em;
        }
    }
    @media all and (max-width: 1534px) {
        #content {
            padding: 0em 7em;
        }
    }
    @media all and (max-width: 1329px) {
        #content {
            padding: 0em 1em;
            flex-direction: column;
            justify-content: center;
            gap: 10em;
        }
    }
    @media all and (max-width: 835px) {
        #content {
            gap: 4em;
            justify-content: center;
            transform: translateY(0em);
        }
    }
</style>