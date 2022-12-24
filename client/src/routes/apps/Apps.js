import { useState } from 'react';
import TextStyles from '../../common/TextStyles';

export default function Apps() {

  const styles = {
    left: {
      display: 'flex',
      flexDirection: 'column',
      gap: '1em',
      height: '100%'
    },
    options: {
      display: 'flex',
      flexDirection: 'column',
      justifyContent: 'center',
      gap: '2em',
      height: '100%'
    },
    p: {
      fontFamily: '-apple-system,system-ui,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial',
      fontSize: "1.8em",
      fontWeight: "normal",
      margin: 0,
      transition: '0.3s ease all',
      color: 'var(--wt)'
    }
  }

  return (
    <div style={styles.left}>
      <h2 style={{...TextStyles.h2, margin: '0 0 -0.1em -0.02em'}}>apps</h2>
      <div style={styles.options}>
        <p style={styles.p}>eatcode</p>
        <p style={styles.p}>portfolio</p>
        <p style={styles.p}>react-native-stripe</p>
        <p style={styles.p}>tsa-pong</p>
        <p style={styles.p}>asd</p>
      </div>
    </div>
  )
}