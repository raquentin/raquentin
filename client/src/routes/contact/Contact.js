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
      <p style={TextStyles.p}>race@racewilliams.com &nbsp;&nbsp;|&nbsp;&nbsp; <InlineDownloadLink fileLink="mailto:race@racewilliams.com" text="general email" /></p>
      <p style={TextStyles.p}>spam@racewilliams.com &nbsp;&nbsp;|&nbsp;&nbsp; <InlineDownloadLink fileLink="mailto:spam@racewilliams.com" text="if you spam, spam honorably" /></p>
      <p style={TextStyles.p}>video@racewilliams.com &nbsp;&nbsp;|&nbsp;&nbsp; <InlineDownloadLink fileLink="mailto:video@racewilliams.com" text="?" /></p>
    </div>
    <div style={styles.options}>
      <p style={TextStyles.p}>#9CQLGGQR &nbsp;&nbsp;-&nbsp;&nbsp; <InlinePageLink to="https://www.clashofstats.com/players/sloppyrace-9CQLGGQR/summary" text="clash" /></p>
      <p style={TextStyles.p}>YungLean &nbsp;&nbsp;-&nbsp;&nbsp; <InlinePageLink to="https://buff.163.com/shop/U1097270818?store_game=csgo#tab=selling&game=csgo" text="buff163" /></p>
      <p style={TextStyles.p}>ChessBoxingMessiah &nbsp;&nbsp;-&nbsp;&nbsp; <InlinePageLink to="https://www.chess.com/member/chessboxingmessiah" text="chess.com" /></p>
    </div> 
  </div>)
}