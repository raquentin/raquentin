import TextStyles from "../../../common/TextStyles"

export default function Eatcode() {
  
  const styles = {
    title: {
      color: '#518a52',
      fontSize: '8em'
    },
    greenPart: {
      color: '#d65a56'
    }
  }   

  return (
    <div style={styles.container}>
      <h2 style={{...TextStyles.h1, ...styles.title}}><span style={styles.greenPart}>eat</span>code</h2>
      <h4 style={TextStyles.h4}>A food-themed technical interview training website for software engineers.</h4>
    </div>

  )
}