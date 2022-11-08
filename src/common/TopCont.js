import TitleSplit from './TitleSplit'

const TopCont = ({title, children}) => {
  const styles = {
    cont: {
      display: 'flex',
      backgroundColor: 'var(--bg)',
      flexDirection: 'column',
      padding: '6em 12em',
      maxHeight: 'calc(100vh - 12em)',
      overflowY: 'scroll'
    }
  };

  return (<>
    <div style={styles.cont}>
      <TitleSplit title={title} />
      {children}
    </div>
  </>);
}

export default TopCont;