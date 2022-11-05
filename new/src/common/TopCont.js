const TopCont = ({children}) => {
  const styles = {
    cont: {
      display: 'flex',
      backgroundColor: 'var(--bg)',
      flexDirection: 'column',
      height: '100%',
      width: '100%',
      gap: '2em',
      gap: '2em'
    }
  };

  return (
      <div style={styles.cont}>
        {children}
      </div>
  );
}

export default TopCont;