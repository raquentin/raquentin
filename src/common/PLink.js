import { useState } from 'react';

const PLink = ({where, text}) => {
  const [isHover, setIsHover] = useState(false);
  const handleMouseEnter = () => {
    setIsHover(true);
  }
  const handleMouseLeave = () => {
    setIsHover(false);
  }

  const styles = {
    link: {
      textDecoration: 'underline',
      textDecorationColor: isHover ? 'var(--ac)' : 'transparent',
      color: isHover ? 'var(--tx)' : 'var(--ac)',
      transition: 'all 0.3s ease',
      fontWeight: 'bold'
    }
  };

  return (
    <a target="_blank" rel="noreferrer" style={styles.link} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave} href={where}>{text}</a>
  );
}

export default PLink;