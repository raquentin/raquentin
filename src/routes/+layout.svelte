<script>

</script>

<div id="main-container">
    <slot />
</div>

<style>
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800;900&family=Roboto+Mono&display=swap');

    :global(body) {
        margin: 0;
        padding: 0;
        width: 100vw;
        height: 100vh;
        background-color: #0D0D0D;
        color: white;
        font-family: 'Inter', sans-serif;
        display: flex;
        align-items: center;
        justify-content: center;
    }
</style>