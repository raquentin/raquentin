import { useState } from 'react';

export default function PullRequest({repo, title, link}) {
	const [hover, setHover] = useState(false);

  const handleMouseEnter = () => {
    setHover(true);
  }
  const handleMouseLeave = () => {
    setHover(false);
  }
	
	const shortTitle = (input) => input.length > 5 ? `${input.substring(0, 29)}...` : input;

	const githubColors = {
		pink: "#ff7b72",
		blue: "#1f6feb",
		dark: "#161b22",
		light: "#30363d",
		lighter: "#8b949e",
		lightest: "#c8d1d9"
	}

	const styles = {
		container: {
			display: 'flex',
			alignItems: 'center',
			gap: '0.3em',
			height: '5.5em',
			padding: '0em 0.7em',
			borderRadius: '0.5em',
			backgroundColor: githubColors.dark,
			border: hover ? `0.2em solid ${githubColors.blue}`: `0.2em solid ${githubColors.light}`,
			transition: '0.3s ease all'
		},
		svg: {
			fill: githubColors.pink,
			width: '2em',
			transform: "translateY(-0.6em)"
		},
		right: {
			display: 'flex',
			flexDirection: 'column',
			gap: '0.6em'
		},
		topRight: {
			display: 'flex',
			gap: '1em'
		},
		repo: {
			color: githubColors.lighter,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			fontWeight: '600',
			fontSize: "1.33em",
			margin: 0
		},
		title: {
			color: githubColors.lightest,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			fontWeight: '600',
			fontSize: "1.33em",
			margin: 0
		},
		created: {
			color: githubColors.lighter,
			fontFamily: '-apple-system,BlinkMacSystemFont,"Segoe UI"',
			fontSize: "1em",
			margin: 0
		}
	}

	return (
		<a onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave} style={styles.container} href={link} target="_blank" rel="noreferrer">
			<svg style={styles.svg} viewBox="0 0 24 24">
					<path d="M 7 2 C 5.3549904 2 4 3.3549904 4 5 C 4 6.292018 4.8442552 7.3942821 6 7.8125 L 6 16.1875 C 4.8442551 16.605718 4 17.707983 4 19 C 4 20.645009 5.3549901 22 7 22 C 8.6450099 22 10 20.645009 10 19 C 10 17.707983 9.1557449 16.605718 8 16.1875 L 8 12.375 C 8.3924774 12.847307 8.8286621 13.277553 9.28125 13.65625 C 10.825446 14.948338 12.592197 15.732229 14.1875 15.9375 C 14.58977 17.12185 15.687419 18 17 18 C 18.64501 18 20 16.64501 20 15 C 20 13.35499 18.64501 12 17 12 C 15.742253 12 14.661408 12.798034 14.21875 13.90625 C 13.137135 13.693644 11.722275 13.095427 10.5625 12.125 C 9.2765576 11.049004 8.2917715 9.5825802 8.0625 7.78125 C 9.1847888 7.3455637 10 6.2679101 10 5 C 10 3.3549904 8.6450096 2 7 2 z M 7 4 C 7.5641294 4 8 4.4358706 8 5 C 8 5.5641294 7.5641294 6 7 6 C 6.4358706 6 6 5.5641294 6 5 C 6 4.4358706 6.4358706 4 7 4 z M 17 14 C 17.564128 14 18 14.435872 18 15 C 18 15.564128 17.564128 16 17 16 C 16.435872 16 16 15.564128 16 15 C 16 14.435872 16.435872 14 17 14 z M 7 18 C 7.5641291 18 8 18.435871 8 19 C 8 19.564129 7.5641291 20 7 20 C 6.4358709 20 6 19.564129 6 19 C 6 18.435871 6.4358709 18 7 18 z"/>
			</svg>
			<div style={styles.right}>
				<div style={styles.topRight}>
					<p style={styles.repo}>{repo}</p>
					<p style={styles.title}>{shortTitle(title)}</p>
				</div>
				<p style={styles.created}>#14 opened 2 weeks ago by r4c3</p>
			</div>
		</a>
	)
}