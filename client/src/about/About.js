import { useState, useEffect } from 'react'

import PageContainer from "../app/PageContainer"
import InlineLink from '../app/InlineLink'

export default function About() {
    const [age, updateAge] = useState(findAge());

    function findAge() { //years between now and birthday to 9 decimal places
      let time = (new Date() - new Date('October 30, 2003 01:30:00')) / (1000 * 60 * 60 * 24 * 365.25);
      return time.toString().substring(0, 12);
    }
  
    useEffect(() => {
      const interval = setInterval(() => updateAge(findAge(), 69));
      return () => clearInterval(interval);
    }, []);

    return (<PageContainer title="hello" content={
        <p>I'm Race Williams, a {age} year-old engineer from Atlanta, GA. My landing page used 
        to be a wall of text lecturing on who I am and what this webapp is. 
        As my site and I grew, I found that writing the AI chatbot below was easier than mantaining 
        the monologue. See <InlineLink to="/contact" text=" /contact" /> to talk to me directly. Enjoy.</p>
    }/>)
}