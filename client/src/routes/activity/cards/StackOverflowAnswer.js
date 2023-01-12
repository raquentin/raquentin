import { useState } from 'react';

export default function StackOverflowAnswer({title, tags, timeAgo, link}) {
	const [hover, setHover] = useState(false);

  const handleMouseEnter = () => {
    setHover(true);
  }
  const handleMouseLeave = () => {
    setHover(false);
  }
	
	const shortTitle = (input) => input.length > 29 ? `${input.substring(0, 26)}...` : input;

	const karma = 4234

	const stackOverflowColors = {
		bg: "#2d2d2d",
		grey: "#acb3b9",
		border: "#404245",
		white: "#c4c8cc",
		tagTextBlue: "#9bc1db",
		tagBgBlue: "#3c4951",
		linkBlue: "#32a7ff"
	}

	const styles = {
		container: {
			position: 'relative',
			display: 'flex',
			flexDirection: 'column',
			gap: '0.8em',
			height: 'min-content',
			padding: '1em',
			backgroundColor: stackOverflowColors.bg,
			border: hover ? '0.1em solid var(--ac)': `0.1em solid ${stackOverflowColors.border}`,
			transition: '0.3s ease all'
		},
		title: {
			color: stackOverflowColors.linkBlue,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			fontSize: "1.6em",
			margin: 0
		},
		tags: {
			display: 'flex',
			gap: '0.6em',
			marginBottom: '3em'
		},
		tag: {
			color: stackOverflowColors.linkBlue,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			margin: 0,
			fontSize: '1em',
			backgroundColor: stackOverflowColors.tagBgBlue,
			color: stackOverflowColors.tagTextBlue,
			padding: '0.3em 0.5em',
			borderRadius: '0.26em'
		},
		img: {
			height: '1.4em',
			transform: 'translateY(0.1em)'
		},
		bottomRight: {
			position: 'absolute',
			alignItems: 'center',
			height: '1.4em',
			bottom: "1em",
			right: "1em",
			gap: '0.7em',
			display: 'flex',
			alignItems: 'flex-end'
		},
		finalText: {
			color: stackOverflowColors.grey,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			fontSize: "1em",
			margin: 0
		}
	}

	return (
		<a onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave} style={styles.container} href={link} target="_blank" rel="noreferrer">
			<p style={styles.title}>{title}</p>
			<div style={styles.tags}>
				{tags.map((tag, i)=>{
					return (
					<p key={i} style={styles.tag}>{tag}</p>
     		);})}
			</div>
			<div style={styles.bottomRight}>
				<img style={styles.img} src="https://pbs.twimg.com/profile_images/1613587840155389958/1FegykbB_400x400.jpg" />
				<p style={styles.finalText}><span style={{color: stackOverflowColors.linkBlue}}>r4c3</span>&nbsp;<span style={{color: stackOverflowColors.white, fontWeight: 'bold'}}>{karma}</span>&nbsp;answered {timeAgo} ago</p>
			</div>
		</a>
	)
}