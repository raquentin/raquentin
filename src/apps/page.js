import TopCont from '../common/TopCont';
import AppCard from './AppCard';
import Data from './data.json';

const Library = () => {
  const styles = {
    grid: {
      display: 'grid',
      gridTemplateColumns: 'repeat(auto-fit, minmax(40em, 3fr))',
      gap: '2em'
    }
  };

  return (
    <TopCont title="apps" children={<>
      <div style={styles.grid}>
        {Data.data.map((resource, i) => (
          <AppCard 
            text={resource.text}
            img={resource.img}
            directLink={resource.directLink}
            key={i}
          />
        ))}
      </div>
    </>} />
  );
};

export default Library;