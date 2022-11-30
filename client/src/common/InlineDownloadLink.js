import { useState } from 'react'

export default function InlineDownloadLink({fileLink, text}) {
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

  return (
    <a download href={fileLink} style={styles.link} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>{text}</a>
  )
}