import { useState, useEffect } from 'react';

import TopCont from '../../common/TopCont';
import PLink from '../../common/PLink'
import Contributor from '../../common/Contributor'

const Eatcode = () => {
  const [openIssuesCount, setOpenIssuesCount] = useState("x");
  const [contributors, setContributors] = useState([{login: "loading..."}]);

  
  useEffect(() => {
    fetch(`https://api.github.com/repos/eatcode-gt/eatcodeweb`)
    .then((response) => response.json())
    .then((data) => {
      setOpenIssuesCount(data.open_issues_count);
    });
   }, []);
  
  useEffect(() => {
    fetch(`https://api.github.com/repos/eatcode-gt/eatcodeweb/contributors`)
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
      backgroundImage: `url(/img/eatcode.gif)`,
      backgroundSize: 'contain',
      backgroundRepeat: 'no-repeat',
      height: '30em',
      marginTop: '0.3em'
    }
  };

  return (
    <TopCont title="eatcode" where="apps" link="/apps" children={<>
      <h4 style={styles.question}>what is eatcode?</h4>
      <p style={styles.response}>Eatcode is is a technical interview training website for software engineers. 
      I'm working on the webapp as Project Manager with a team of six other Georgia Tech CS students in 
      association with GT Web Dev club. Eatcode's unique 3D food-themed UI, relatively fast solution testing, 
      and $0 pricetag push it beyond its more popular competitors like Leetcode (🤮) 
      and HackerRank (🤢). Eatcode is not yet a live app, but you can track our development&nbsp;
      <PLink where="https://github.com/eatcode-gt/eatcodeweb" text="on Github" />.
      </p>

      <h4 style={styles.question}>can I contribute?</h4>
      <p style={styles.response}>Yes, eatcode currently has {openIssuesCount} open issue(s) awaiting an open-source contribution 
      from someone like you. All issues are enumerated on the <PLink where="https://github.com/eatcode-gt/eatcodeweb/issues" text="issues page" />
      . If it's your first time contributing, consider searching for issues with the "good first issue" label.</p>
      
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