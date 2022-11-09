import { useState } from 'react';

const Contributor = ({contributor, i}) => {
  const [isHover, setIsHover] = useState(false);
  const handleMouseEnter = () => {
    setIsHover(true);
  }
  const handleMouseLeave = () => {
    setIsHover(false);
  }

  const styles = {
    link: {
      color: isHover ? 'var(--ac)' : 'var(--tx)',
      transition: 'all 0.3s ease',
      paddingBottom: '0.4em'
    }
  };

  return (
    <a style={styles.link} key={i} href={contributor.html_url} target="_blank" rel="noreferrer" onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}><p>{i+1}.) {contributor.login}</p></a>
  );
}

export default Contributor;