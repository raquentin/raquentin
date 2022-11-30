

export default function PageContainer({title, content}) {
    const styles = {
        container: {
            height: 'minContent',
            display: 'flex',
            flexDirection: 'column',
            gap: '2em',
            width: '78vw'
        }
    }

    return (
        <div style={styles.container}>
            <h2>{title}</h2>
            {content}
        </div>
    )
}