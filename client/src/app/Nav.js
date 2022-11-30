

import Link from "./Link"

export default function Nav() {

    const styles = {
        container: {
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            justifyContent: 'center',
            gap: '6em',
            maxHeight: 'calc(100% - 14em)',
            width: '11.75vw',
            flex: '1'
        },
        links: {
            display: 'flex',
            flexDirection: 'column',
            justifyContent: 'center',
            alignItems: 'start',
            width: '100%',
            gap: '2.3em',
        }
    }

    return (
        <div style={styles.container}>
            <h1>Race Williams</h1>
            <div style={styles.links}>
                <Link to="/" text="about" isActive={false} />
                <Link to="/resume" text="resume" isActive={false} />
                <Link to="/projects" text="projects" isActive={false} />
                <Link to="/library" text="library" isActive={false} />
                <Link to="/russian" text="russian" isActive={false} />
                <Link to="/posts" text="posts" isActive={false} />
                <Link to="/schedule" text="schedule" isActive={false} />
                <Link to="/activity" text="activity" isActive={false} />
                <Link to="/contact" text="contact" isActive={false} />
            </div>
        </div>
    )
}