
import TopCont from '../common/TopCont';

const Landing = () => {
  const styles = {
    sectionTitle: {
        color: 'var(--tx)',
        marginBottom: '-0.3em'
    },
    roleTitle: {
        color: 'var(--tx)',
        margin: '0.9em 0 0.3em 0.4em'
        
    },
    roleDesc: {
        color: 'var(--tx)',
        margin: '0 0 0.2em 1em'
    },
    span: {
        color: 'var(--ac',
        fontWeight: 'bold'
    }
  };

  return (
    <TopCont title="resume" children={<>
        <h3 style={styles.sectionTitle}>Education</h3>
            <p style={styles.roleTitle}><span style={styles.span}>• Georgia Institute of Technology</span> &nbsp;(Aug 2022 - Dec 2025 expected)</p>
                <p style={styles.roleDesc}>• BS/MS Candidate in Computer Science, BS Candidate in Applied Russian Language and Culture.</p>
                <p style={styles.roleDesc}>• Gold Scholarship recipient; 4.00 GPA.</p>
            <br /><br />
        <h3 style={styles.sectionTitle}>Work Experience</h3>
            <p style={styles.roleTitle}><span style={styles.span}>• NPM Properties</span> &nbsp;Assistant Systems Administrator (May 2022 - Aug 2022)</p>
                <p style={styles.roleDesc}>• Created a more appealing and accessible interface for tornado alert app TornadoFree by applying UI/UX principles to a Figma mockup and a React Native mobile app. This app helps renters inside and outside of NPM Properties to seek shelter from tornadoes during warnings.</p>
                <p style={styles.roleDesc}>• Improved present, and future company networks by documenting Systems Administrator tasks so future employees can better assume the position.</p>
            <p style={styles.roleTitle}><span style={styles.span}>• Harris County Board of Education</span> &nbsp;Live-streaming Intern (Mar 2021 - Jul 2022)</p>
                <p style={styles.roleDesc}>•  Live-streamed Board of Education meetings on Zoom and YouTube to a total of 7000 local viewers over 15
                    months by operating cameras, computers, and other audiovisual devices.</p>
            <p style={styles.roleTitle}><span style={styles.span}>• Sheepa</span> &nbsp;Owner (May 2021 - Aug 2022)</p>
                <p style={styles.roleDesc}>• Turned an initial investment of $200 to more than $12,000 in total sales by selling refurbished electronics.</p>
                <p style={styles.roleDesc}>• Increased sales over time by analyzing competitor pricing, maintaining 100% positive reviews, and promptly
                    responding to questions from customers.</p>
            <br /><br />
        <h3 style={styles.sectionTitle}>Projects and Leadership</h3>
            <p style={styles.roleTitle}><span style={styles.span}>• Eatcode</span> &nbsp;Project Manager</p>
                <p style={styles.roleDesc}>• Leading a team of six Georgia Tech CS students to develop Eatcode, a software engineering technical interview question training website.</p>
                <p style={styles.roleDesc}>• Assigning front and back-end tasks to myself and team members based on our strengths and weaknesses, merging pull requests and having other members merge mine, organizing weekly team meetings, monitoring progress with respect to deadlines, and more.</p>
            <p style={styles.roleTitle}><span style={styles.span}>• RaceWilliams.com</span> &nbsp;Creator, etc.</p>
                <p style={styles.roleDesc}>• Changing the definition of the online portfolio by combining open source projects with a library of free
                    resources ranging from music production loops, electronic drum-kits, ASMR, and video reviews of different
                    water brands.</p>
                <p style={styles.roleDesc}>• Amassed over 15,000 downloads of music loops by producers and 11,000 views across various YouTube
                    channels.</p>
            <p style={styles.roleTitle}><span style={styles.span}>• STEAMBreak</span> &nbsp;Web Developer, Creator, Fundraiser, Everything</p>
                <p style={styles.roleDesc}>• Increased K-12 STEAM education in my county in spite of Coronavirus by creating a virtual jeopardy-style Capture the Flag competition.</p>
                <p style={styles.roleDesc}>• Wrote %gt;200 challenges for students in topics including art history, Python, chemistry, future career options, steganography, and more.</p>
                <p style={styles.roleDesc}>• Future competitions held via my software company Porkana.com.</p>
            <br /><br />
        <h3 style={styles.sectionTitle}>Skills</h3>
            <p style={styles.roleTitle}><span style={styles.span}>• Programming Technologies:</span> &nbsp;MongoDB, ExpressJS, NodeJS, ReactJS, PyQt, SvelteKit, Figma, GIMP, Git,
                HTML/CSS/JS, Java, Python, TypeScript, React Native, GitHub, Windows, Unix, Nginx, VPSs</p>
            <p style={styles.roleTitle}><span style={styles.span}>• Relavant Courses:</span> &nbsp;Object-Oriented Programming (Java), Computer Science Principles (Python)</p>
            <br /><br />   
        <h3 style={styles.sectionTitle}>Public Presentations</h3>
            <p style={styles.roleTitle}><span style={styles.span}>• Georgia Technology Student Association:</span> &nbsp;Lectured on how codebases become world-changing applications.</p>
            <p style={styles.roleTitle}><span style={styles.span}>• TEDxGeorgiaTech:</span> &nbsp;Stand-up comedy act about computer science majors.</p>
    </>} />
  );
};

export default Landing;