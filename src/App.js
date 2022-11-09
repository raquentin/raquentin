import { PageTransition } from '@steveeeie/react-page-transition';
import { BrowserRouter as Router, Routes, Route, useLocation } from 'react-router-dom';

import './common/fonts.css';

import Landing from './landing/page';
import Apps from './apps/page';
  import Eatcode from './apps/eatcode/page'
import Library from './library/page';
import Daily from './daily/page';
import Resume from './resume/page';
import Contact from './contact/page';

function App() {
  let styles = {
    app: {
      '--bg': '#242424', //background
      '--tx': '#FFFFFF', //text
      '--ac': '#8adb65', //accent
      height: '100vh',
      width: '100vw',
      backgroundColor: 'var(--bg)',
    },
    container: {
      overflowY: 'auto !important'
    }
  }

  const location = useLocation();
  return (
    <main style={styles.app}>
      <PageTransition style={styles.container} preset="scaleDownScaleDown" transitionKey={location.key}>
        <Routes location={location}>
          <Route exact path='/' element={<Landing />} />
          <Route exact path='/apps' element={<Apps />} />
            <Route exact path='/apps/eatcode' element={<Eatcode />} />
          <Route exact path='/library' element={<Library />} />
          <Route exact path='/ежедневный' element={<Daily />} />
          <Route exact path='/resume' element={<Resume />} />
          <Route exact path='/contact' element={<Contact />} />
        </Routes>
      </PageTransition>
    </main>
  );
}

const Root = () => <Router><App /></Router>;

export default Root;