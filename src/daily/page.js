import { useState, useEffect } from "react";
import { Player } from 'video-react';

import TopCont from '../common/TopCont';

const Daily = () => {
  const [videos, setVideos] = useState([]);


  useEffect(() => {
    fetch('https://api.imgur.com/3/gallery.json', {
      headers: {
        'Authorization': 'Client-ID 3899eeb02cf3ac0'
      }
    })
    .then((response) => console.log(response.json()));
   }, []);

  const styles = {
    video: {
      backgroundImage: "url('https://external-preview.redd.it/7XlmFivxD_dHNR82G8M9PDdn6RfqZWohYOqDFQtXuwA.png?width=640&crop=smart&format=pjpg&auto=webp&s=456a3ef2c21b06f8ca0d1fe995341f8dfef9314a')",
      height: '30em',
      width: ''
    }
  };

  return (
    <TopCont title="ежедневный" children={<>
      {/* {videos.map((video, i) => (
        <video key={i}
          src={video.data.url} >
        </video>
      ))} */}
    </>} />
  );
};

export default Daily;