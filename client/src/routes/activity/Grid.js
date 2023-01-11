import { useEffect, useState } from 'react';
import axios from 'axios';

import TextStyles from "../../common/TextStyles";
import InlinePageLink from "../../common/InlinePageLink"
import PullRequest from './cards/PullRequest';
import StackOverflowAnswer from './cards/StackOverflowAnswer';
import Tweet from './cards/Tweet';

export default function Grid() {
  const [githubItems, setGithubItems] = useState([])
  const [stackOverflowItems, setStackOverflowItems] = useState([])
  const [twitterItems, setTwitterItems] = useState([])

  useEffect(() => {
    axios({
      method: 'GET',
      url: 'http://localhost:3002/api/activity',
      headers: { 
        'Content-Type': 'application/json'
      },
    }).then((response) => {
      if (response.data != []) {
        setGithubItems(response.data[1])
        setStackOverflowItems(response.data[0])
        setTwitterItems(response.data[3])
      }
      console.log(response.data)
    })
  }, [])

	const styles = {
		text: {
			display: 'flex',
			flexDirection: 'column',
			gap: '2em',
			marginBottom: '2em'
		},
		feed: {
			flex: 2,
			display: 'flex',
			gap: '2em',
			overflowY: 'scroll'
		},
		column: {
			display: 'flex',
			flexDirection: 'column',
			gap: '2em',
			height: 'max-content',
			flex: 1,
			marginBottom: '5vh'
		}
	}

	return (<>
		<div style={styles.text}>
			<h2 style={{...TextStyles.h2, margin: '0 0 -0.4em -0.02em'}}>activity</h2>
			<p style={TextStyles.p}>My recent activity on {<InlinePageLink to="https://github.com/r4c3" text="GitHub" />}
			, {<InlinePageLink to="https://stackoverflow.com/users/20668816/race-williams" text="Stack Overflow" />},
			and {<InlinePageLink to="https://twitter.com/r4c3" text="Twitter" />} all in one place.</p>
		</div>
		<div style={styles.feed}>
			<div style={styles.column}>
        {githubItems && 
        githubItems.map((item, i) => {
          console.log(item)
          return (
            <PullRequest key={i} repo={item.repo} title={item.commitMessage} createdAt={item.createdAt}/>
          )
        })}
			</div>
			<div style={styles.column}>
        {stackOverflowItems && 
        stackOverflowItems.map((item, i) => {
          console.log(item)
          return (
            <StackOverflowAnswer key={i} title={item.title} link={item.share_link} tags={["adding", "this", "soon"]} timeAgo={"some time"} rep={item.reputation}/>
          )
        })}
			</div>
			<div style={styles.column}>
        {twitterItems && 
        twitterItems.map((item, i) => {
          return (
            <Tweet key={i} text={item.text}/>
          )
        })}
			</div>
		</div>
	</>)
}