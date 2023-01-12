import { BrowserRouter as Router, Routes, Route } from 'react-router-dom'
import { useState } from 'react'

import { ThemeContext } from './Contexts'

import AnimationLayout from './AnimationLayout'
import Nav from './Nav'
import About from '../routes/about/About'
import Activity from '../routes/activity/Activity'
import Contact from '../routes/contact/Contact'
import Articles from '../routes/articles/Articles'
import Apps from '../routes/apps/Apps'
import AppsRouter from '../routes/apps/AppsRouter'
import Russian from '../routes/russian/Russian'

import './app.css'

const App = () => {
  const [isLightMode, setIsLightMode] = useState(false)
  const themeContextValue = { isLightMode, setIsLightMode }


  let styles = {
    app: {
      '--bk': isLightMode ? '#dadfe3' : '#111111', //light bg : dark bg
      '--wt': isLightMode ? '#000000' : '#ffffff', //black : white
      '--gr': isLightMode ? '#111111' : '#3f3f3f', //off black : off white
      '--ac':  isLightMode ? '#723bad' : '#70cee6', //light accent : dark accent
      height: '100vh',
      width: '90vw',
      backgroundColor: 'var(--bk)',
      display: 'inline-flex',
      alignItems: 'center',
      padding: '0 5vw',
      gap: '5em',
      transition: 'all 0.3s ease',
      color: 'var(--wt)'
    }
  }

  return (
    <ThemeContext.Provider value={themeContextValue}>
      <main style={styles.app}>
        <Nav />
        <Routes>
          <Route element={<AnimationLayout />}>
            <Route path="/" element={<About />} />
            <Route path="/activity" element={<Activity />} />
            <Route path="/contact" element={<Contact />} />
            <Route path="/articles" element={<Articles />} />
            <Route path="/apps" element={<Apps />} />
            <Route path="/apps/:id" element={<AppsRouter />} />
            {/* <Route path="/russian" element={<Russian />} /> */}
          </Route>
        </Routes>
      </main>
    </ThemeContext.Provider>
  );
}

const Root = () => <Router><App /></Router>;

export default Root;