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
import Projects from '../routes/projects/Projects'
import Russian from '../routes/russian/Russian'
import Schedule from '../routes/schedule/Schedule'

import './app.css'

const App = () => {
  const [isLightMode, setIsLightMode] = useState(false)
  const themeContextValue = { isLightMode, setIsLightMode }


  let styles = {
    app: {
      '--bk': isLightMode ? '#dadfe3' : '#10171c', //very light blue : very dark blue
      '--wt': isLightMode ? '#000000' : '#ffffff', //black : white
      '--gr': isLightMode ? '#000000' : '#e2e2e2', //black : off white
      '--ac':  isLightMode ? '#0f537d' : '#44a6e3', //darkish blue : lightish blue
      height: '100vh',
      width: '90vw',
      backgroundColor: 'var(--bk)',
      display: 'inline-flex',
      alignItems: 'center',
      padding: '0 5vw',
      gap: '5em',
      transition: 'all 0.3s ease',
      '@media (max-width: 1494px)': {
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
            <Route path="/projects" element={<Projects />} />
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