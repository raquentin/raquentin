import TextStyles from '../common/TextStyles'
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
      gap: '2.6em',
      width: '11.75vw',
      flex: '1'
    },
    icons: {
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'space-between',
      width: '100%',
      marginBottom: '.4em'
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
        <h1 style={TextStyles.h1}>Race Williams</h1>
        <div style={styles.icons}>
          <NavIcon which="ToggleTheme" />
          <NavIcon which="LinkedIn" />
          <NavIcon which="StackOverflow" />
          <NavIcon which="Twitter" />
          <NavIcon which="GitHub" />
        </div>
        <div style={styles.links}>
          <LinkNav to="/" text="about" isActive={false} />
          <LinkNav to="/activity" text="activity" isActive={false} />
          <LinkNav to="/apps" text="apps" isActive={false} />
          <LinkNav to="/articles" text="articles" isActive={false} />
          <LinkNav to="/contact" text="contact" isActive={false} />
          <LinkNav to="/russian" text="русский" isActive={false} />
        </div>
      </div>
      <Divider />
    </div>
  )
}