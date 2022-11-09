import { useState, useEffect } from 'react';

import TopCont from '../../common/TopCont';
import PLink from '../../common/PLink'
import Contributor from '../Contributor'

const Eatcode = () => {
  const [openIssuesCount, setOpenIssuesCount] = useState("x");
  const [contributors, setContributors] = useState([{login: "loading..."}]);
  
  useEffect(() => {
    fetch(`https://api.github.com/repos/r4c3/portfolio/contributors`)
    .then((response) => response.json())
    .then((data) => {
      setContributors(data);
    });
   }, []);
   
  console.log(contributors)

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
      backgroundImage: `url(/img/portfolio.gif)`,
      backgroundSize: 'contain',
      backgroundRepeat: 'no-repeat',
      height: '30em',
      marginTop: '0.3em'
    }
  };

  return (
    <TopCont title="portfolio" where="apps" link="/apps" children={<>
      <h4 style={styles.question}>what is racewilliams.com?</h4>
      <p style={styles.response}>The site you're on right now. It's a lot of things; it hosts my open source applications, library resources, daily ежедневный videos, resume, and more.
      </p>

      <h4 style={styles.question}>can I contribute?</h4>
      <p style={styles.response}>Kind of? Anyone can fork this portfolio repo to use as their own, though any contributions made on any individual fork will not
      influence racewilliams.com.</p>
      
      <div style={styles.bottom}>
        <div style={styles.bottomSide}>
          <h4 style={styles.question}>live demo</h4>
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

export default Eatcode;