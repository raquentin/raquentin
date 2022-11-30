import { BrowserRouter as Router, Routes, Route } from 'react-router-dom'

import AnimationLayout from './AnimationLayout'
import Nav from './Nav'
import About from '../routes/about/About'
import Contact from '../routes/contact/Contact'
import Library from '../routes/library/Library'
import Posts from '../routes/posts/Posts'
import Projects from '../routes/projects/Projects'
import Resume from '../routes/resume/Resume'
import Russian from '../routes/russian/Russian'
import Schedule from '../routes/schedule/Schedule'
import Activity from '../routes/activity/Activity'

import './app.css'

const App = () => {
  let styles = {
    app: {
      '--bk': '#020202', //black
      '--wt': '#FFFFFF', //white
      '--gr': '#E2E2E2', //white
      '--ac': '#FF7043', //accent
      height: '100vh',
      width: 'calc(90vw)',
      backgroundColor: 'var(--bk)',
      display: 'inline-flex',
      alignItems: 'center',
      padding: '0 5vw',
      gap: '5em'
    }
  }

  return (
      <main style={styles.app}>
        <Nav />
        <Routes>
          <Route element={<AnimationLayout />}>
            <Route path="/" element={<About />} />
            <Route path="/resume" element={<Resume />} />
            <Route path="/projects" element={<Projects />} />
            <Route path="/library" element={<Library />} />
            <Route path="/russian" element={<Russian />} />
            <Route path="/posts" element={<Posts />} />
            <Route path="/schedule" element={<Schedule />} />
            <Route path="/activity" element={<Activity />} />
            <Route path="/contact" element={<Contact />} />
          </Route>
        </Routes>
      </main>
  );
}

const Root = () => <Router><App /></Router>;

export default Root;