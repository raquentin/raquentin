import { useState } from 'react';
import { Link } from 'react-router-dom';

const LibraryCard = ({text, img, directLink}) => {
  const [isHover, setIsHover] = useState(false);
  const handleMouseEnter = () => {
    setIsHover(true);
  }
  const handleMouseLeave = () => {
    setIsHover(false);
  }

  const styles = {
    cont: {
      cursor: 'pointer',
      position: 'relative',
      backgroundColor: 'var(--ac)',
      border: '.3em solid var(--ac)',
      backgroundSize: 'cover',
      aspectRatio: '1',
    },
    external: {
      position: 'absolute',
      opacity: isHover ? 1 : 0,
      fontWeight: 'bold',
      textAlign: 'center',
      margin: '50% 50%',
      transform: 'translate(-50%, -50%)',
      transition: 'all 0.42s ease',
      zIndex: '1',
      width: '100%'
    },
    externalText: {
      color: 'var(--ac)',
      backgroundColor: 'rgba(0, 0, 0, .5)',
      padding: '0.2em 0'
    },
    bg: {
      backgroundSize: 'cover',
      height: '98%',
      transition: 'all 0.42s ease',
      backgroundImage: `url(./img/${img})`,
      filter: isHover ? 'invert(100%)' : 'none',
      zIndex: '-1'
    },
    text: {
      color: 'black',
      padding: '0.18em',
      transform: 'translateY(0.07em)'
    }
  };

  return (
    <>
    {directLink === ''
    ? <Link to={`/${text}`} style={styles.cont} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>
        <div style={styles.bg} alt="image for lib item" />
        <p style={styles.text}>{text}</p>
      </Link>
    : <a href={`${directLink}`} style={styles.cont} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>
        <div style={styles.external}><p style={styles.externalText}>this links to an external page</p></div>
        <div style={styles.bg} alt="image for lib item" />
        <p style={styles.text}>{text}</p>
      </a>
    }</>
  );
};

export default LibraryCard;