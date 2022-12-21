export default function PageContainer({pathname, children}) {
  const styles = {
    container: {
      height: 'minContent',
      maxHeight: '80vh',
      overflowY: 'scroll',
      width: '70vw'
    }
  }

  return (
    <div style={styles.container}>
      {children}
    </div>
  )
}