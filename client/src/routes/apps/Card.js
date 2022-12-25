import { useState } from 'react'
import { Link } from 'react-router-dom'


export default function Card({to, text, disabled}) {
	const [hover, setHover] = useState(false);

	const handleMouseEnter = () => {
		setHover(true);
	}
	const handleMouseLeave = () => {
		setHover(false);
	}

	const styles = {
		a: {
			border: disabled ? '0.3em solid var(--gr)' : (hover ? '0.3em solid var(--ac)' : '0.3em solid var(--wt)'),
			height: '100%',
			backgroundColor: 'var(--bl)',
			width: '100%',
			transition: 'all 0.3s ease',
			display: 'flex',
			alignItems: 'center',
			justifyContent: 'center',
			cursor: disabled ? 'initial' : 'pointer'
			// aspectRatio: (16/9)
		},
		p: {
			fontFamily: '-apple-system,system-ui,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial',
			fontSize: "3em",
			fontWeight: "bold",
			margin: 0,
			transition: '0.3s ease all',
			color: disabled ? 'var(--gr)' : 'var(--wt)',
		}
	}

	return (
		<Link to={`/apps${to}`} style={styles.a} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>
			<p style={styles.p}>{text}</p>
		</Link>
	)
}