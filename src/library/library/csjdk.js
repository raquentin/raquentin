import { useState, useEffect } from 'react';

import TopCont from '../../common/TopCont';
import PLink from '../../common/PLink'
import Contributor from '../../common/Contributor'

const CSJDK = () => {
    const [contributors, setContributors] = useState([{login: "loading..."}]);
  
    useEffect(() => {
      fetch(`https://api.github.com/repos/r4c3/drumkits/contributors`)
      .then((response) => response.json())
      .then((data) => {
        setContributors(data);
      });
     }, []);

  const styles = {
    question: {
      color: 'var(--ac)',
      marginBottom: '0.3em'
    },
    response: {
      color: 'var(--tx)',
      marginBottom: '2em'
    },
    link: {
      color: 'var(--ac)',
      fontWeight: 'bold'
    },
    bottom: {
      display: 'flex',
      gap: '3em',
      width: ''
    },
    bottomSide: {
      flex: '50%',
      display: 'flex',
      flexDirection: 'column',
    },
    demo: {
      backgroundImage: `url(/img/csj.jpg)`,
      backgroundSize: 'contain',
      backgroundRepeat: 'no-repeat',
      height: '30em',
      marginTop: '0.3em'
    }
  };

  return (
    <TopCont key={'csj'} title="csj dk" where="library" link="/library" children={<>
      <h4 style={styles.question}>communist slow jams drum kit</h4>
      <p style={styles.response}>Contains over 100 chops from JPEGMAFIA's Communist Slow Jams. 
      </p>

      <h4 style={styles.question}>can I contribute?</h4>
      <p style={styles.response}>Yes. All racewilliams.com drumkits are open source. Add your own chops or edit existing ones <PLink where="https://github.com/r4c3/drumkits" text="on Github" />.</p>
      <div style={styles.bottom}>
        <div style={styles.bottomSide}>
          <h4 style={styles.question}>drumkit cover</h4>
          <div style={styles.demo} alt="asd" />
        </div>
        <div style={styles.bottomSide}>
          <h4 style={styles.question}>top contributors</h4>
          {contributors.slice(0,9).map((contributor, i) => (
            <Contributor contributor={contributor} i={i}/>
          ))}
        </div>
      </div>
    </>} />
  );
};

export default CSJDK;