import { useState } from 'react'
import { NavLink } from 'react-router-dom'

export default function LinkNav({to, text}) { //not to be confused with NavLink
  const [hover, setHover] = useState(false);

  const handleMouseEnter = () => {
    setHover(true);
  }
  const handleMouseLeave = () => {
    setHover(false);
  }

  function returnStyles(isActive) {
    return ({
      cursor: 'pointer',
      color: isActive ? 'var(--ac)' : 'var(--wt)',
      fontWeight: isActive ? 'bold' : 'normal',
      borderLeft: isActive ? '0em solid transparent' : (hover ? '0.8em solid var(--ac)' : '0em solid transparent'),
      paddingLeft: isActive ? '0em' : (hover ? '0.48em' : '0em'),
      transition: 'all 0.3s ease',
      width: 'min-content'
    })
  }

  const styles = {
    inherit: {
      color: 'inherit',
      fontWeight: 'inherit',
    }
  }

  return ( //https://reactrouter.com/en/main/components/nav-link
    <NavLink to={to} style={({isActive}) => returnStyles(isActive)}><h3 style={styles.inherit} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>{text}</h3></NavLink>
  )
}