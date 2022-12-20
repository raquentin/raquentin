import Divider from './Divider'
import LinkNav from "./LinkNav"
import NavIcon from './NavIcon'

export default function Nav() {

  const styles = {
    container: {
      width: '15vw',
      maxHeight: 'calc(100% - 14em)',
      height: 'calc(100% - 14em)',
      gap: '5em',
      alignItems: 'center',
      display: 'flex'
    },
    notDivider: {
      display: 'flex',
      flexDirection: 'column',
      alignItems: 'center',
      justifyContent: 'center',
      gap: '3em',
      width: '11.75vw',
      flex: '1'
    },
    icons: {
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'space-between',
      width: '90%'
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
      <div style={styles.notDivider}>
        <h1>Race Williams</h1>
        <div style={styles.icons}>
          <NavIcon which="GitHub" />
          <NavIcon which="LinkedIn" />
          <NavIcon which="ToggleTheme" />
        </div>
        <div style={styles.links}>
          <LinkNav to="/" text="about" isActive={false} />
          <LinkNav to="/activity" text="activity" isActive={false} />
          <LinkNav to="/contact" text="contact" isActive={false} />
          <LinkNav to="/library" text="library" isActive={false} />
          <LinkNav to="/posts" text="posts" isActive={false} />
          <LinkNav to="/projects" text="projects" isActive={false} />
          <LinkNav to="/russian" text="russian" isActive={false} />
          <LinkNav to="/schedule" text="schedule" isActive={false} />
        </div>
      </div>
      <Divider />
    </div>
  )
}