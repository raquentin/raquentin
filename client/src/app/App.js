import { BrowserRouter as Router, Routes, Route } from 'react-router-dom'

import Nav from './Nav'
import Divider from './Divider'
import About from '../about/About'
import Contact from '../contact/Contact'
import Library from '../library/Library'
import Posts from '../posts/Posts'
import Projects from '../projects/Projects'
import Resume from '../resume/Resume'
import Russian from '../russian/Russian'
import Schedule from '../schedule/Schedule'
import Activity from '../activity/Activity'

import './app.css'

function App() {
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
      gap: '4.5em'
    }
  }

  return (
    <main style={styles.app}>
      <Nav />
      <Divider />
      <Routes>
        <Route path="/" element={<About />} />
        <Route path="/resume" element={<Resume />} />
        <Route path="/projects" element={<Projects />} />
        <Route path="/library" element={<Library />} />
        <Route path="/russian" element={<Russian />} />
        <Route path="/posts" element={<Posts />} />
        <Route path="/schedule" element={<Schedule />} />
        <Route path="/activity" element={<Activity />} />
        <Route path="/contact" element={<Contact />} />
      </Routes>
    </main>
  );
}

const Root = () => <Router><App /></Router>;

export default Root;