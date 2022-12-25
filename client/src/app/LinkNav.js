import { useState } from 'react'
import { NavLink } from 'react-router-dom'
import TextStyles from '../common/TextStyles';


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
      borderLeft: isActive ? (hover ? '0.8em solid var(--bk)' : '0em solid transparent') : (hover ? '0.8em solid var(--ac)' : '0em solid transparent'),
      paddingLeft: isActive ? (hover ? '0.48em' : '0em') : (hover ? '0.48em' : '0em'),
      width: 'min-content',
      transition: 'all 0.3s ease'
    })
  }

  const styles = {
    inherit: {
      color: 'inherit',
      fontWeight: 'inherit',
    }
  }

  return ( //https://reactrouter.com/en/main/components/nav-link
    <NavLink end to={to} style={({isActive}) => returnStyles(isActive)}><h3 style={{...TextStyles.h3, ...styles.inherit}} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>{text}</h3></NavLink>
  )
}