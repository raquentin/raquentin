import { useState } from 'react'
import { Link } from 'react-router-dom'

export default function InlinePageLink({to, text}) {
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

  if (to.charAt(0) === '/') { //* it references a racewilliams.com route
    return (
      <Link to={to} style={styles.link} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>{text}</Link>
    )
  } else { //* it references an external link
    return (
      <a href={to} target="_blank" rel="noreferrer" style={styles.link} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>{text}</a>
    )
  }

}