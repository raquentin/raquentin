import { useState } from 'react';

export default function Tweet({text, date, link}) {
	const [hover, setHover] = useState(false);

  const handleMouseEnter = () => {
    setHover(true);
  }
  const handleMouseLeave = () => {
    setHover(false);
  }

	const twitterColors =  {
		black: "#000000",
		dark: "#2f3336",
		light: "#6b7074",
		lightest: "#e7e9ea",
		blue: "#1d9bf0"
	}

	const styles = {
		container: {
			backgroundColor: twitterColors.black,
			border: hover ? '0.15em solid var(--ac)': `0.15em solid ${twitterColors.dark}`,
			transition: '0.3s ease all',
			gap: '1.2em',
			padding: '1.4em 1em',
			height: 'min-content',
			borderRadius: '0.2em',
			display: 'flex',
			flexDirection: 'column'
		},
		img: {
			height: '4em',
			borderRadius: '100px'
		},
		top: {
			display: 'flex',
			gap: '1em',
			alignItems: 'center'
		},
		topTexts: {
			display: 'flex',
			flexDirection: 'column',
			gap: '0.2em',
			transform: 'translateY(-0.2em)'
		},
		name: {
			color: twitterColors.lightest,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			fontWeight: '700',
			fontSize: "1.27em",
			margin: 0
		},
		handle: {
			color: twitterColors.light,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			fontSize: "1.2em",
			margin: 0
		},
		text: {
			color: twitterColors.lightest,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			fontSize: "1.8em",
			margin: 0
		},
		date: {
			color: twitterColors.light,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			fontSize: "1.2em",
			margin: 0
		}
	}

	return (
		<a onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave} style={styles.container} href={link} target="_blank" rel="noreferrer">
			<div style={styles.top}>
				<img style={styles.img} src="https://pbs.twimg.com/profile_images/1605380577225490433/kVUkKCTk_400x400.jpg" />
				<div style={styles.topTexts}>
					<p style={styles.name}>Race</p>
					<p style={styles.handle}>@racewilliamscom</p>
				</div>
			</div>
			<p style={styles.text}>{text}</p>
			<p style={styles.date}>{date}</p>
		</a>
	)
}