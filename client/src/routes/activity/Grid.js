// import { useState } from 'react';
// import axios from 'axios';

import TextStyles from "../../common/TextStyles";
import InlinePageLink from "../../common/InlinePageLink"
import PullRequest from './cards/PullRequest';
import StackOverflowAnswer from './cards/StackOverflowAnswer';
import Tweet from './cards/Tweet';

export default function Grid() {

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
				<PullRequest repo="r4c3/portfolio" title="Updated Russian page" link="https://github.com/eatcode-gt/eatcodeweb/pull/70"/>
				<StackOverflowAnswer title="нщгк ьщь" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
				<Tweet text="These boxes are hardcoded for now" date="6:39 PM · Dec 20, 2022" link="https://twitter.com/racewilliamscom/status/1605347222362914817"/>
				<StackOverflowAnswer title="Very verbose long lengthy extra large title for no reason what is the charmax on these thingys thingamabobers" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
				<PullRequest repo="r4c3/repo200" title="Longer pr title that is annoying but ok" link="https://github.com/eatcode-gt/eatcodeweb/pull/70"/>
				<PullRequest repo="r4c3/react-native" title="ed" link="https://github.com/eatcode-gt/eatcodeweb/pull/70"/>
				<StackOverflowAnswer title="нщгк ьщь" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
			</div>
			<div style={styles.column}>
				<StackOverflowAnswer title="How to use axios to fetch StackOverflow api user answers" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
				<Tweet text="tweet example" date="6:39 PM · Dec 20, 2022" link="https://twitter.com/racewilliamscom/status/1605347222362914817"/>
				<StackOverflowAnswer title="How to use axios to fetch StackOverflow api user answers" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
				<StackOverflowAnswer title="Problem with tailwind css" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
				<PullRequest repo="r4c3/eatcodeweb" title="Added worker README" link="https://github.com/eatcode-gt/eatcodeweb/pull/70"/>
				<StackOverflowAnswer title="Problem with tailwind css" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
				<PullRequest repo="r4c3/eatcodeweb" title="Added worker README" link="https://github.com/eatcode-gt/eatcodeweb/pull/70"/>
				<StackOverflowAnswer title="нщгк ьщь" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
			</div>
			<div style={styles.column}>
				<StackOverflowAnswer title="нщгк ьщь" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
				<Tweet text="ideally this is a text string of one hundred and 40 characters. merry crihmih. living off borrowed time the clock ticks f" date="6:39 PM · Dec 20, 2022" link="https://twitter.com/racewilliamscom/status/1605347222362914817"/>
				<StackOverflowAnswer title="нщгк ьщь" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
				<Tweet text="ideally this is a text string of one hundred and 40 characters. merry crihmih. living off borrowed time the clock ticks f" date="6:39 PM · Dec 20, 2022" link="https://twitter.com/racewilliamscom/status/1605347222362914817"/>
				<StackOverflowAnswer title="Very verbose long lengthy extra large title for no reason what is the charmax on these thingys thingamabobers" link="https://stackoverflow.com/a/74659516/20668816" tags={["react", "javascript", "css", "mern-stack"]} timeAgo="3 hours"/>
				<PullRequest repo="r4c3/repo200" title="Longer pr title that is annoying but ok" link="https://github.com/eatcode-gt/eatcodeweb/pull/70"/>
				<PullRequest repo="r4c3/react-native" title="ed" link="https://github.com/eatcode-gt/eatcodeweb/pull/70"/>
			</div>
		</div>
	</>)
}