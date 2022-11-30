export default function Divider() {
    const styles = {
        divider: {
            width: '0.25vw',
            height: 'calc(100% - 14em)',
            backgroundColor: 'var(--gr)'
        }
    }

    return (
        <div style={styles.divider} />
    )
}