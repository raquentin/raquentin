import TextStyles from '../../common/TextStyles';
import Card from './Card';

export default function Apps() {

  const styles = {
    left: {
      display: 'grid',
      flexDirection: 'column',
      gap: '1em',
      height: 'calc(100% - 4em)',
      width: 'calc(100% - 4em)',
      gridTemplateColumns: '1fr 1fr 1fr',
      gap: '2em',
    },
  }

  return (
    <div style={styles.left}>
        <Card to="/eatcode" text="Eatcode" gifUrl="https://google.com" />
        <Card to="/portfolio" text="Portfolio" gifUrl="https://google.com" disabled/>
        <Card to="/millerpong" text="MillerPong" gifUrl="https://google.com" disabled/>
        <Card to="/mfchess" text="mfChess" gifUrl="https://google.com" disabled/>
        <Card to="/greeneq" text="GreenEQ" gifUrl="https://google.com" disabled/>
        <Card to="/drumsonline" text="drums.online" gifUrl="https://google.com" disabled/>
    </div>
  )
}