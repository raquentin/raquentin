
import TopCont from '../common/TopCont';
import LibraryCard from './LibraryCard';
import Data from './data.json';

const Library = () => {
  const styles = {
    grid: {
      display: 'grid',
      gridTemplateColumns: 'repeat(auto-fit, minmax(13em, 5fr))',
      gap: '2em'
    }
  };

  return (
    <TopCont title="library" children={<>
      <div style={styles.grid}>
        {Data.data.map((resource, i) => (
          <LibraryCard 
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