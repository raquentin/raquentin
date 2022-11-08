import { useState } from 'react';

const ViewSource = ({where, text}) => {
  const [isHover, setIsHover] = useState(false);
  const handleMouseEnter = () => {
    setIsHover(true);
  }
  const handleMouseLeave = () => {
    setIsHover(false);
  }

  const styles = {
    text: {
      color: isHover ? 'var(--tx)' : 'black',
      transition: 'all 0.3s ease',
    }
  };

  return (
    <a href={where}><h4 style={styles.text} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>{text}</h4></a>
  );
}

export default ViewSource;