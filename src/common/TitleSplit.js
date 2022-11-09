import { useState } from 'react';
import { Link } from 'react-router-dom';

const TitleSplit = ({title, where, link}) => {
  const [isHover, setIsHover] = useState(false);
  const handleMouseEnter = () => {
    setIsHover(true);
  }
  const handleMouseLeave = () => {
    setIsHover(false);
  }
 
  const styles ={
    cont: {
      display: 'flex',
      justifyContent: 'space-between',
      flexDirection: 'column',
      width: '100%'
    },
    title: {
      color: 'var(--ac)'
    },
    goBack: {
      fontWeight: 'bold',
      color: isHover ? 'var(--ac)' : 'var(--tx)',
      transition: 'all 0.3s ease',
      marginBottom: '1.5em'
    }
  }
  
  return (
    <div style={styles.cont}>
      <h1 style={styles.title}>{title}</h1>
      <Link style={styles.goBack} to={link} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}><p>&larr; back to {where} page</p></Link>
    </div>
  );
}

export default TitleSplit;