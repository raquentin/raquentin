export default function PageContainer({pathname, children}) {
  const styles = {
    container: {
      height: 'minContent',
      maxHeight: '80vh',
      overflow: 'hidden',
      maxWidth: '70vw'
    }
  }

  return (
    <div style={styles.container}>
      {children}
    </div>
  )
}