import { useState } from 'react'
import { Link } from 'react-router-dom'

export default function InlineLink({to, text}) {
    const [hover, setHover] = useState(false);
    const handleMouseEnter = () => {
      setHover(true);
    }
    const handleMouseLeave = () => {
      setHover(false);
    }

    const styles = {
      link: {
        color: hover ? 'var(--ac)' : 'var(--wt)',
        fontWeight: 'bold',
        transition: 'all 0.3s ease',
        width: 'min-content',
        fontSize: 'inherit'
      }
    }

    return ( //https://reactrouter.com/en/main/components/nav-link
        <Link to={to} style={styles.link} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>{text}</Link>
    )
}