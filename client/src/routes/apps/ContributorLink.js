import { useState } from "react";

import TextStyles from "../../common/TextStyles"

export default function ContributorLink({name, commits, i}) {
  const [hover, setHover] = useState(false);

  const handleMouseEnter = () => {
    setHover(true);
  }
  const handleMouseLeave = () => {
    setHover(false);
  }

  const styles = {
    link: {
      color: hover ? "var(--ac)" : "var(--wt)"
    }
  }

  return (
    <a key={i} onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave} href={`https://github.com/${name}`} target="_blank" rel="noreferrer" style={styles.link}><p style={TextStyles.p}>{i+1}: {name} ({commits} commits)</p></a>
  )
}