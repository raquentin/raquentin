import { useState, useContext } from 'react'

import { ThemeContext } from './Contexts';

export default function ThemeLink({which}) {
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
    icon: {
      width: '2em',
      height: '2em',
      fill: hover ? 'var(--ac)' : 'var(--wt)',
      transition: '0.3s ease all',
      cursor: 'pointer'
    }
  }

  switch (which) {
    case "GitHub":
      return ( //* GitHub SVG Icon
        <a href="https://github.com/r4c3" target="_blank" rel="noreferrer"><svg onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave} style={styles.icon} version="1.1" viewBox="0 0 16 16">
          <path d="M8 0.198c-4.418 0-8 3.582-8 8 0 3.535 2.292 6.533 5.471 7.591 0.4 0.074 0.547-0.174 0.547-0.385 0-0.191-0.008-0.821-0.011-1.489-2.226 0.484-2.695-0.944-2.695-0.944-0.364-0.925-0.888-1.171-0.888-1.171-0.726-0.497 0.055-0.486 0.055-0.486 0.803 0.056 1.226 0.824 1.226 0.824 0.714 1.223 1.872 0.869 2.328 0.665 0.072-0.517 0.279-0.87 0.508-1.070-1.777-0.202-3.645-0.888-3.645-3.954 0-0.873 0.313-1.587 0.824-2.147-0.083-0.202-0.357-1.015 0.077-2.117 0 0 0.672-0.215 2.201 0.82 0.638-0.177 1.322-0.266 2.002-0.269 0.68 0.003 1.365 0.092 2.004 0.269 1.527-1.035 2.198-0.82 2.198-0.82 0.435 1.102 0.162 1.916 0.079 2.117 0.513 0.56 0.823 1.274 0.823 2.147 0 3.073-1.872 3.749-3.653 3.947 0.287 0.248 0.543 0.735 0.543 1.481 0 1.070-0.009 1.932-0.009 2.195 0 0.213 0.144 0.462 0.55 0.384 3.177-1.059 5.466-4.057 5.466-7.59 0-4.418-3.582-8-8-8z"></path>
        </svg></a>
      );
    case "LinkedIn":
      return ( //* LinkedIn SVG Icon
        <a href="https://linkedin.com/in/r4c3" target="_blank" rel="noreferrer"><svg onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave} style={styles.icon} version="1.1" viewBox="0 0 16 16">
          <path d="M6 6h2.767v1.418h0.040c0.385-0.691 1.327-1.418 2.732-1.418 2.921 0 3.461 1.818 3.461 4.183v4.817h-2.885v-4.27c0-1.018-0.021-2.329-1.5-2.329-1.502 0-1.732 1.109-1.732 2.255v4.344h-2.883v-9z"></path>
          <path d="M1 6h3v9h-3v-9z"></path>
          <path d="M4 3.5c0 0.828-0.672 1.5-1.5 1.5s-1.5-0.672-1.5-1.5c0-0.828 0.672-1.5 1.5-1.5s1.5 0.672 1.5 1.5z"></path>
        </svg></a>
      );
    case "ToggleTheme":
      return ( //* ToggleTheme SVG Icon
        <svg onClick={toggleIsLightMode} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave} style={styles.icon} version="1.1" viewBox="0 0 16 16">
          <path d="M4.055 8c0-1.022 0.829-1.851 1.851-1.851s1.851 0.829 1.851 1.851c0 1.022-0.829 1.851-1.851 1.851s-1.851-0.829-1.851-1.851zM8 0c-4.418 0-8 3.582-8 8s3.582 8 8 8c4.418 0 8-3.582 8-8s-3.582-8-8-8zM5.928 14.989c-2.406-1.4-4.023-4.005-4.023-6.989s1.617-5.589 4.023-6.989c2.406 1.399 4.025 4.005 4.025 6.989s-1.618 5.589-4.025 6.989z"></path>
        </svg>
      );
    default:
      console.error("An invalid { where } prop was passed.")
  }
}