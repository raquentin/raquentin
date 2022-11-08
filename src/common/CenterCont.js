const CenterCont = ({children}) => {
  const styles = {
    cont: {
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'space-between',
      height: '100%',
      padding: '0em 12em',
      gap: '2em',
      transform: 'translateY(-3.5em)'
    }
  };

  return (
    <div style={styles.cont}>
      {children}
    </div>
  );
}

export default CenterCont;