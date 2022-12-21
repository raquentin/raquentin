import { BrowserRouter as Router, Routes, Route } from 'react-router-dom'
import { useState } from 'react'

import { ThemeContext } from './Contexts'

import AnimationLayout from './AnimationLayout'
import Nav from './Nav'
import About from '../routes/about/About'
import Activity from '../routes/activity/Activity'
import Contact from '../routes/contact/Contact'
import Library from '../routes/library/Library'
import Posts from '../routes/posts/Posts'
import Apps from '../routes/apps/Apps'
import Russian from '../routes/russian/Russian'
import Schedule from '../routes/schedule/Schedule'

import './app.css'

const App = () => {
  const [isLightMode, setIsLightMode] = useState(false)
  const themeContextValue = { isLightMode, setIsLightMode }


  let styles = {
    app: {
      '--bk': isLightMode ? '#dadfe3' : '#111111', //light bg : dark bg
      '--wt': isLightMode ? '#000000' : '#ffffff', //black : white
      '--gr': isLightMode ? '#111111' : '#e2e2e2', //off black : off white
      '--ac':  isLightMode ? '#468260' : '#fa6e66', //light accent : dark accent
      height: '100vh',
      width: '90vw',
      backgroundColor: 'var(--bk)',
      display: 'inline-flex',
      alignItems: 'center',
      padding: '0 5vw',
      gap: '5em',
      transition: 'all 0.3s ease',
      '@media (maxWidth: 1494px)': {
        gap: '2em',
        width: '96vw',
      }
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
            <Route path="/library" element={<Library />} />
            <Route path="/posts" element={<Posts />} />
            <Route path="/apps" element={<Apps />} />
            <Route path="/russian" element={<Russian />} />
            <Route path="/schedule" element={<Schedule />} />
          </Route>
        </Routes>
      </main>
    </ThemeContext.Provider>
  );
}

const Root = () => <Router><App /></Router>;

export default Root;