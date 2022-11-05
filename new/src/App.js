import { PageTransition } from '@steveeeie/react-page-transition';
import { BrowserRouter as Router, Routes, Route, useLocation } from 'react-router-dom';
import { useState, useEffect } from 'react';

import './common/fonts.css';

import Landing from './landing/page';

function App() {
  const [theme, setTheme] = useState(false);
  const toggleTheme = () => {
    setTheme(!theme);
  };

  let styles = {
    app: {
      '--bg': theme ? '#D6D6D6' : '#242424', //background (off-white : off-black)
      '--tx': theme ? '#000000' : '#FFFFFF', //text (black : white)
      '--ac': theme ? '#FF783E' : '#F490B4', //accent (orange : orange)
      height: '100vh',
      width: '100vw',
      backgroundColor: 'var(--bg)',
    }
  };

  const location = useLocation();
  return (
    <main style={styles.app}>
      <PageTransition preset='moveToLeftFromRight' transitionKey={location.key}>
        <Routes location={location}>
          <Route exact path='/' element={<Landing />} />
        </Routes>
      </PageTransition>
    </main>
  );
}

const Root = () => <Router><App /></Router>;
export default Root;