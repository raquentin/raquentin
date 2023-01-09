import { useState, useEffect } from 'react'
import axios from 'axios'

import ContributorLink from '../ContributorLink'
import TextStyles from "../../../common/TextStyles"
import demo from './demo720p.mov'
import InlinePageLink from '../../../common/InlinePageLink'

export default function Eatcode() {
  const [topContributors, setTopContributors] = useState(["Loading..."])

  useEffect(() => {
    axios({
      url: "https://api.github.com/repos/eatcode-gt/eatcodeweb/contributors",
      method: "GET"
    }).then((res) => {
      setTopContributors(res.data)
    }).catch((err) => {
      console.error(err)
    })
  }, [])

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
      maxWidth: '30vw',
    },
    topCommits: {
      flex: 1,
      height: '100%',
      display: 'flex',
      flexDirection: 'column',
      gap: '0.2em'
    },
    lowerHalf: {

    }
  }   

  return (
    <div style={styles.container}>
      <div style={styles.top}>
        <h2 style={{...TextStyles.h1, ...styles.title}}><span style={styles.greenPart}>eat</span>code</h2>
        <h4 style={TextStyles.h4}>A food-themed technical interview training website for software engineers.</h4>
      </div>
      <div style={styles.bottom}>
        <video style={styles.vid} autoPlay controls loop>
          <source src={demo} type="video/mp4" />
        </video>
        <div style={styles.topCommits}>
          <h4 style={{...TextStyles.h4, marginBottom: '0.3em'}}>Top Contributors:</h4>
          {topContributors.map((contributor, i) => {
            return (
              <ContributorLink key={i} i={i} name={contributor.login} commits={contributor.contributions} />
            )
          })}
        </div>
      </div>
      <div style={styles.lowerHalf}>
        <h4 style={{...TextStyles.h4, marginBottom: '0.1em'}}>Want to contribute?</h4>
        <p style={TextStyles.p}>Eatcode is always looking for contributions.
        Visit the <InlinePageLink to="https://github.com/eatcode-gt/eatcodeweb/issues" text="issues page"/> and
        look for issues tagged "good first issue" to get started.</p>
      </div>
    </div>
  )
}