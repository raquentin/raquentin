export default function PageContainer({pathname, children}) {
  const styles = {
    container: {
      height: 'minContent',
      maxHeight: '80vh',
      height: '70vw',
      display: 'flex',
      justifyContent: 'center',
      flexDirection: 'column',
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