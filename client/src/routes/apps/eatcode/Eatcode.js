import { useState } from 'react'
import axios from 'axios'

import TextStyles from "../../../common/TextStyles"
import demo from './demo720p.mov'

export default function Eatcode() {
  const [topContributors, setTopContributors] = useState([])


  axios.get(`https://api.github.com/repos/eatcode-gt/eatcodeweb/contributors`)
    .then((res))
    .then((data) => {
      setTopContributors(data);
    });
   };

  const styles = {
    container: {
      maxHeight: '100%',
      display: 'flex',
      flexDirection: 'column',
      gap: '3em'
    },
    title: {
      color: '#518a52',
      fontSize: '8em'
    },
    greenPart: {
      color: '#d65a56'
    },
    bottom: {
      display: 'flex',
      justifyContent: 'space-between',
      gap: '3em'
    },
    vid: {
      flex: 1,
      maxWidth: '30vw'
    },
    topCommits: {
      flex: 1
    }
  }   

  return (
    <div style={styles.container}>
      <div>
        <h2 style={{...TextStyles.h1, ...styles.title}}><span style={styles.greenPart}>eat</span>code</h2>
        <h4 style={TextStyles.h4}>A food-themed technical interview training website for software engineers.</h4>
      </div>
      <div style={styles.bottom}>
        <video style={styles.vid} controls autoPlay loop>
          <source src={demo} type="video/mp4" />
        </video>
        <div style={styles.topCommits}>
          <h4 style={TextStyles.h4}>Top Contributors:</h4>
          {topContributors.map((contributor, i) => {
            <p style={TextStyles.p}>{i}: {contributor}</p>
          })}
        </div>
      </div>
    </div>

  )
}