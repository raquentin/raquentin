import { useState } from 'react';
import { Link } from 'react-router-dom';

import ViewSource from './ViewSource';

const AppCard = ({text, img}) => {
  const [isHover, setIsHover] = useState(false);
  const handleMouseEnter = () => {
    setIsHover(true);
  }
  const handleMouseLeave = () => {
    setIsHover(false);
  }

  const styles = {
    cont: {
      position: 'relative',
      backgroundColor: 'var(--tx)',
      backgroundSize: 'cover',
      aspectRatio: '2',
      cursor: 'default'
    },
    bg: {
      cursor: 'pointer',
      backgroundSize: 'cover',
      height: '98%',
      backgroundColor: 'var(--ac)',
      backgroundImage: `url(/img/${img})`,
      transition: 'all 0.4s ease',
      filter: isHover ? 'invert(57%) sepia(74%) saturate(411%) hue-rotate(61deg) brightness(96%) contrast(89%)' : 'none',
      zIndex: '-1'
    },
    bottomBar: {
      backgroundColor: isHover ? 'var(--tx)' : 'var(--ac)',
      transition: 'all 0.4s ease',
      display: 'flex',
      justifyContent: 'space-between',
      padding: '0.4em 0.8em 0.54em 0.8em',
      alignItems: 'center'
    },
    text: {
      color: 'black'
    }
  };

  return (
    <Link to={`/apps/${text}`} style={styles.cont}>
      <div style={styles.bg} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave} alt="image for lib item" />
      <div style={styles.bottomBar}>
        <h3 style={styles.text}>{text}</h3>
        <ViewSource where={"https://github.com"} text={"view source"}/>
      </div>
    </Link>
  );
};

export default AppCard;