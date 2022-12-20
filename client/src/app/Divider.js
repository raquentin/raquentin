export default function Divider() {
  const styles = {
    divider: {
      minWidth: '0.25vw',
      height: '100%',
      backgroundColor: 'var(--gr)',
      transition: '0.3s ease all'
    }
  }

  return (
    <div style={styles.divider} />
  )
}