import { useState } from 'react';

const Link = ({where, text}) => {
  const [isHover, setIsHover] = useState(false);
  const handleMouseEnter = () => {
    setIsHover(true);
  }
  const handleMouseLeave = () => {
    setIsHover(false);
  }

  const styles = {
    link: {
      borderRadius: '1em',
      padding: '0.7em 1.9em'
    },
    text: {
      textDecoration: 'underline',
      textDecorationColor: isHover ? 'var(--ac)' : 'transparent',
      color: isHover ? 'var(--tx)' : 'var(--ac)',
      transition: 'all 0.3s ease'
    }
  };

  return (
    <>{where == 'toggleTheme'
    ? <div style={styles.link} href={where}><h3 style={styles.text} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>{text}</h3></div>
    : <a style={styles.link} href={where}><h3 style={styles.text} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>{text}</h3></a>}</>
  );
}

export default Link;