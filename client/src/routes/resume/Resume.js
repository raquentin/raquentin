import InlineDownloadLink from '../../common/InlineDownloadLink'

export default function Resume() {
  const styles = { //resume font sizes go against app.css
    container: {
      display: 'flex',
      gap: '3em',
      alignItems: 'center'
    },
    contentStrip: {
      flex: 1,
      display: 'flex',
      flexDirection: 'column',
      gap: '3em'
    },
    section: {
      display: 'flex',
      flexDirection: 'column',
      gap: '0.4em'
    },
    h2: {
      fontSize: '2.8em',
      fontWeight: 'normal'
    },
    h3: {
      fontSize: '2em',
      fontWeight: 'bold',
      letterSpacing: '1px'
    },
    h4: {
      fontSize: '2em',
      fontWeight: 'normal'
    },
    h5: {
      fontSize: '2em',
      fontWeight: 'normal',
      color: 'var(--ac)'
    },
    h5Special: {
      fontSize: '1em',
      fontWeight: 'bold',
    }
  }

  return (<>
    <p>View the webified version below, or download the PDF version <InlineDownloadLink fileLink="/contact" text="here" />.</p>
    <div style={styles.container}>
      <div style={styles.contentStrip}>
        <div style={styles.section}>
          <h2 style={styles.h2}>Education</h2>
          <h3 style={styles.h3}>Georgia Institute of Technology</h3>
          <h5 style={styles.h5}>exp. Dec 2025 | Atlanta, GA</h5>
          <div> {/*blank div cancels flex gap from section*/}
            <p>BS in Computer Science</p>
            <p>BS in Russian</p>
            <p>MS in Computer Science</p>
          </div>
          <br /> {/*separates mini-sections*/}
          <div> {/*blank div cancels flex gap from section*/}
            <p>Gold Scholar</p>
            <p>Townsend Scholarship Recipient</p>
          </div>
        </div>
        <div style={styles.section}>
          <h2 style={styles.h2}>Work Experience</h2>
          <h3 style={styles.h3}>NPM Properties</h3>
          <h4 style={styles.h4}>Assistant SysAdmin & SWE Intern</h4>
          <h5 style={styles.h5}>May 2022 - Aug 2022 | Columbus, GA</h5>
          <p>❖ Designed and implemented new UI with React Native and Figma.</p>
          <p>❖ Documented SysAdmin tasks to help future employees assume the position.</p>
          <br /> {/*separates mini-sections*/}
          <h3 style={styles.h3}>Harris County BoE</h3>
          <h4 style={styles.h4}>Live-streaming Intern</h4>
          <h5 style={styles.h5}>Mar 2021 - Jul 2022 | Cataula, GA</h5>
          <p>❖ Streamed Board of Education meetings to &gt;7000 local viewers over 15 months by operating cameras, computers, and mics.</p>
        </div>
      </div>
      <div style={styles.contentStrip}>
        <div style={styles.section}>
          <h2 style={styles.h2}>Business Projects</h2>
          <h3 style={styles.h3}>Porkana</h3>
          <h5 style={styles.h5}>Mar 2021 - eternity | WWW</h5>
          <p>❖ Incorporated Porkana to begin selling software more professionally.</p>
          <p>❖ Held 2 (soon 3) Porkana Olympiad competitions which reward K-12 students for solving STEAM problems.</p>
          <br /> {/*separates mini-sections*/}
          <h3 style={styles.h3}>Sheepa</h3>
          <h5 style={styles.h5}>May 2021 - Aug 2022 | Cataula, GA</h5>
          <p>❖ Turned a $200 investment into &gt;$6000 profit (~$11000 gross) by reselling electronics.</p>
          <p>❖ Increased sales over time by analyzing competitor pricing, maintaining 100% positive reviews, and promptly responding customer inquiries.</p>
        </div>
        {/* <div style={styles.section}>
          <h2 style={styles.h2}>Skills</h2>
          <h3 style={styles.h3}>Technical</h3>
          <p>React, MongoDB, Express, Node, PyQt, SvelteKit, Figma, Gimp, Git, Java, Python, React Native, Nginx, Spline, + more.</p>
          <br />
          <h3 style={styles.h3}>Interpersonal</h3>
          <p><span style={styles.h5Special}>Presenting: </span>gave CS lecture at state high school tech conference, publicly demoed open-source projects.</p>
          <p><span style={styles.h5Special}>Brain-collaging: </span>merged ideas (and prs) as project manager open-source projects, made this term up.</p>
        </div> */}
      </div>
      <div style={styles.contentStrip}>
        <div style={styles.section}>
          <h2 style={styles.h2}>Technical Projects</h2>
          <h3 style={styles.h3}>CS Algorithms Site (Eatcode)</h3>
          <h5 style={styles.h5}>Sep 2022 - eternity | Atlanta, GA</h5>
          <p>❖ Working as project manager with six GT CS students to create a software engineering technical interview question training website with React, MongoDB, Express, and Node.</p>
          <p>❖ Merging pull requests from both team members and random open source contributors, holding weekly stand-up meetings, and settings feature deadlines with GitHub issues to meet project expectations.</p>
          <br /> {/*separates mini-sections*/}
          <h3 style={styles.h3}>Web Portfolio</h3>
          <h5 style={styles.h5}>April 2020 - eternity | WWW</h5>
          <p>❖ Wrote and deployed personal website hosting open-source applications, various free resources totaling 15,000 downloads and 11,000 views, daily Russian videos, and more.</p>
        </div>
      </div>
    </div>
  </>)
}