import { useState, useEffect } from "react";
import HoverVideoPlayer from 'react-hover-video-player';

import TopCont from '../common/TopCont';

const Daily = () => {
  const [videos, setVideos] = useState([
    {date: "11/09/22", link: "https://i.imgur.com/zDsF42Y.mp4"},
    {date: "11/10/22", link: ""},
    {date: "11/11/22", link: ""},
    {date: "11/12/22", link: ""},
    {date: "11/13/22", link: ""},
    {date: "11/14/22", link: ""},
    {date: "11/15/22", link: ""},
    {date: "11/16/22", link: ""}
  ]);

  const styles = {
    cont: {
      display: 'grid',
      gridTemplateColumns: 'repeat(auto-fit, minmax(22em, 4fr))',
      gap: '2em'
    },
    videoCont: {
      width: '20em',
      height: `${(16 / 9 * 20) + 2.5}em`,
      padding: '0.3em',
      backgroundColor: 'var(--ac)'
    },
    noVideo: {
      width: '20em',
      backgroundColor: 'var(--bg)',
      height: `${(16 / 9 * 20) + 0.1}em`,
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center'
    },
    comingSoon: {
      color: 'var(--ac)',
      transform: 'translateY(-0.5em)'
    },
    text: {
      color: 'black',
      fontWeight: 'bold',
      transform: 'translateY(0.2em)'
    }
  };

  return (
    <TopCont title="ежедневный" children={
    <div style={styles.cont}>
      {videos.map((video, i) => (
        <div style={styles.videoCont}>
          {video.link == ""
          ? <div style={styles.noVideo}><h3 style={styles.comingSoon}>скоро будет</h3></div>
          : <HoverVideoPlayer muted={false} key={i} videoSrc={video.link}/>
          }
          <p key={i} style={styles.text}>{video.date}</p>
        </div>
      ))}
    </div>
    } />
  );
};

export default Daily;