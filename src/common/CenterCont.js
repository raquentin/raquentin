import TitleSplit from "./TitleSplit";

const CenterCont = ({title, where, link, children}) => {
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
      {title === 0
      ? <></> //Landing page doesn't need TitleSplit
      : <TitleSplit title={title} where={where} link={link}/>
      }
      {children}
    </div>
  );
}

export default CenterCont;