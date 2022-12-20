

export default function PageContainer({pathname, children}) {
  
  const pathnameTitleMap = {
    "/": "hello",
    "/about": "about",
    "/activity": "activity",
    "/contact": "contact",
    "/library": "library",
    "/posts": "posts",
    "/projects": "projects",
    "/resume": "resume",
    "/russian": "russian",
    "/schedule": "schedule",
  }
  
  const styles = {
    container: {
      height: 'minContent',
      display: 'flex',
      flexDirection: 'column',
      gap: '1em',
      width: '70vw',
      transition: '0.3s ease all'
    }
  }

  return (
    <div style={styles.container}>
      <h2>{pathnameTitleMap[pathname]}</h2>
      {children}
    </div>
  )
}