import TextStyles from "../../common/TextStyles"
import InlinePageLink from "../../common/InlinePageLink"
import InlineDownloadLink from "../../common/InlineDownloadLink"

export default function Contact() {

  const styles = {
    container: {
      display: 'flex',
      flexDirection: 'column',
      gap: '1.2em'
    },
    options: {
      display: 'flex',
      flexDirection: 'column',
      gap: '0.3em'
    }
  }

  return (
  <div style={styles.container}>
    <h2 style={{...TextStyles.h2, margin: '0 0 -0.1em -0.02em'}}>contact</h2>
    <div style={styles.options}>
      <h3 style={TextStyles.h3}>email</h3>
      <p style={TextStyles.p}><InlineDownloadLink fileLink="mailto:race@racewilliams.com" text="race@racewilliams.com" /></p>
      <p style={TextStyles.p}><InlineDownloadLink fileLink="mailto:spam@racewilliams.com" text="spam@racewilliams.com" /></p>
    </div>
    <div style={styles.options}>
    <h3 style={TextStyles.h3}>other</h3>
      <p style={TextStyles.p}><InlinePageLink to="https://linkedin.com/in/r4c3" text="linkedin" /></p>
      <p style={TextStyles.p}><InlinePageLink to="https://github.com/r4c3" text="github" /></p>
      <p style={TextStyles.p}><InlinePageLink to="https://stackoverflow.com/users/20668816/race-williams" text="stack overflow" /></p>
      <p style={TextStyles.p}><InlinePageLink to="https://twitter.com/racewilliamscom" text="twitter" /></p>
      <p style={TextStyles.p}><InlinePageLink to="https://buff.163.com/shop/U1097270818?store_game=csgo#tab=selling&game=csgo" text="buff 163" /></p>
      <p style={TextStyles.p}><InlinePageLink to="https://www.chess.com/member/chessboxingmessiah" text="chess.com" /></p>
    </div> 
  </div>)
}