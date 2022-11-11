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
    read: {
      color: 'var(--ac)'
    }
  };

  return (
    <CenterCont title={0} children={<>
      <div style={styles.info}>
        <h1 style={styles.title}>racewilliams.com</h1>
        <p style={styles.p}>
          Hello! I'm Race Williams, a <span style={styles.age}>{age}</span> year-old student and software engineer interested in human-centered web design 
          and languages — both spoken and programming. This webapp hosts some of my open-source apps, my resume, and a library of free resources ranging from industrial 
          hip-hop drumkits to flashcards for learning Russian skateboarding vocabulary. Learn more via the links to the right.
        </p>

        <br/>
        <p style={styles.p}><span style={styles.read}>Read:</span> I'm rewriting my site so it's going to be annoying to use for a bit. I've
        scheduled the main issues to be resolved by Dec 1. I give you permission to ddos me and/or spam my email with botted messages of rage if the site still breaks after then.</p>
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