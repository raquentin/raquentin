import { useState, useEffect } from 'react';

import PageLink from './PageLink';
import CenterCont from '../common/CenterCont';

const Landing = () => {
  const [age, updateAge] = useState(findAge());

  function findAge() { //years between now and birthday to 9 decimal places
    let time = (new Date() - new Date('October 30, 2003 01:30:00')) / (1000 * 60 * 60 * 24 * 365.25);
    return time.toString().substring(0, 12);
  }

  useEffect(() => {
    const interval = setInterval(() => updateAge(findAge(), 69));
    return () => clearInterval(interval);
  }, []);

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
      <div style={styles.info}>
        <h1 style={styles.title}>racewilliams.com</h1>
        <p style={styles.p}>
          Hello! I'm Race Williams, a <span style={styles.age}>{age}</span> year-old student and software engineer interested in human-centered web design 
          and languages — both spoken and programming. This webapp hosts some of my open-source apps, my resume, and a library of free resources ranging from industrial 
          hip-hop drumkits to flashcards for learning Russian skateboarding vocabulary. Learn more via the links to the right.
        </p>

        <br/>
        <p style={styles.p}>msg: Site is in a transition period now as I transfer it from SvelteKit to React. Library page, apps page, ежедневный, and responsiveness should be fully reinstated by Dec 1.</p>
      </div>
      <div style={styles.links}>
        <PageLink where="/apps" text="apps" />
        <PageLink where="/library" text="library" />
        <PageLink where="/ежедневный" text="ежедневный" />
        <PageLink where="/resume" text="resume" />
        <PageLink where="/contact" text="contact" />
      </div>
    </>} />
  );
};

export default Landing;