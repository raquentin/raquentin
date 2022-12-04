import { useState, useContext } from 'react'

import { ThemeContext } from './Contexts';

export default function ThemeLink() {
  const { isLightMode, setIsLightMode } = useContext(ThemeContext)
  const [hover, setHover] = useState(false);

  const handleMouseEnter = () => {
    setHover(true);
  }
  const handleMouseLeave = () => {
    setHover(false);
  }

  const toggleIsLightMode = () => {
    setIsLightMode(isLightMode ? false : true)
  }

  const styles = {
    link: {
      cursor: 'pointer',
      color: 'var(--wt)',
      borderLeft: hover ? '0.8em solid var(--ac)' : '0em solid transparent',
      paddingLeft: hover ? '0.48em' : '0em',
      transition: 'all 0.3s ease'
    },
    inherit: {
        color: 'inherit',
        fontWeight: 'inherit',
      }
  }

  return (
    <div style={styles.link} onClick={toggleIsLightMode}><h3 style={styles.inherit} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>lights</h3></div>
  )
}