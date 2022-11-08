import CenterCont from '../common/CenterCont';
import ContactLink from './ContactLink';
import TitleSplit from '../common/TitleSplit'

const Contact = () => {
  const styles = {
    info: {
      flex: '50%',
    },
    title: {
      color: 'var(--ac)',
      marginLeft: '-0.05em'
    },
    p: {
      color: 'var(--tx)',
      maxWidth: '26em',
      inlineSize: '90vw',
      fontWeight: 'bold'
    },
    age: {
      color: 'var(--ac)'
    },
    links: {
      flex: '50%',
      maxWidth: '40%',
      display: 'flex',
      flexDirection: 'column',
      alignItems: 'end',
      gap: '0.8em'
    },
  };

  return (
    <CenterCont children={<>
      <TitleSplit title={"contact"}/>
      <div style={styles.links}>
        <ContactLink style={styles.fakePageLink} where="mailto:rаce@racewilliams.com" text="rаce@racewilliams.com" />
        <ContactLink style={styles.fakePageLink} where="mailto:race@gatech.edu" text="race@gatech.edu" />
        <ContactLink style={styles.fakePageLink} where="https://discord.com/users/raise#2848" text="raise#2848" />
        <ContactLink style={styles.fakePageLink} where="https://www.linkedin.com/in/r4c3/" text="/in/r4c3" />
      </div>
    </>} />
  );
};

export default Contact;