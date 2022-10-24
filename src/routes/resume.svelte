<script>
    import ContentTop from "../comps/ContentTop.svelte"
</script>

<ContentTop>
    <div>
        <div id="titleSplit">
            <div>
                <h1>resume</h1>
                <a href="/"><p>&larr; back to landing page</p></a>
            </div>
            <a class="navBtn" download href="Race_Williams_Resume.pdf"><h2 style="transform: translateY(-0.02em);">download PDF</h2></a>
        </div>
        <div id="resume">
            <h3>Education</h3>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Georgia Institute of Technology</span> &nbsp;(Aug 2022 - Dec 2026 expected)</p>
                    <p class="roleDesc">•  BS/MS Candidate in Computer Science, BS Candidate in Applied Russian Language and Culture.</p>
                    <p class="roleDesc">• Gold Scholarship recipient; 4.00 GPA.</p>
                <br><br>
            <h3>Work Experience</h3>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• NPM Properties</span> &nbsp;Assistant Systems Administrator (May 2022 - Aug 2022)</p>
                    <p class="roleDesc">• Created a more appealing and accessible interface for tornado alert app TornadoFree by applying UI/UX principles to a Figma mockup and a React Native mobile app. This app helps renters inside and outside of NPM Properties to seek shelter from tornadoes during warnings.</p>
                    <p class="roleDesc">• Improved present, and future company networks by documenting Systems Administrator tasks so future employees can better assume the position.</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Harris County Board of Education</span> &nbsp;Live-streaming Intern (Mar 2021 - Jul 2022)</p>
                    <p class="roleDesc">•  Live-streamed Board of Education meetings on Zoom and YouTube to a total of 7000 local viewers over 15
                        months by operating cameras, computers, and other audiovisual devices.</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Sheepa</span> &nbsp;Owner (May 2021 - Aug 2022)</p>
                    <p class="roleDesc">• Turned an initial investment of $200 to more than $12,000 in total sales by selling refurbished electronics.</p>
                    <p class="roleDesc">• Increased sales over time by analyzing competitor pricing, maintaining 100% positive reviews, and promptly
                        responding to questions from customers.</p>
                <br><br>
            <h3>Projects and Leadership</h3>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Eatcode</span> &nbsp;Project Manager</p>
                    <p class="roleDesc">• Leading a team of six Georgia Tech CS students to develop Eatcode, a software engineering technical interview question training website.</p>
                    <p class="roleDesc">• Assigning front and back-end tasks to myself and team members based on our strengths and weaknesses, merging pull requests and having other members merge mine, organizing weekly team meetings, monitoring progress with respect to deadlines, and more.</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• RaceWilliams.com</span> &nbsp;Creator, etc.</p>
                    <p class="roleDesc">• Changing the definition of the online portfolio by combining open source projects with a library of free
                        resources ranging from music production loops, electronic drum-kits, ASMR, and video reviews of different
                        water brands.</p>
                    <p class="roleDesc">• Amassed over 15,000 downloads of music loops by producers and 11,000 views across various YouTube
                        channels.</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• STEAMBreak</span> &nbsp;Web Developer, Creator, Fundraiser, Everything</p>
                    <p class="roleDesc">• Increased K-12 STEAM education in my county in spite of Coronavirus by creating a virtual jeopardy-style Capture the Flag competition.</p>
                    <p class="roleDesc">• Wrote >200 challenges for students in topics including art history, Python, chemistry, future career options, steganography, and more.</p>
                    <p class="roleDesc">• Future competitions held via my software company Porkana.com.</p>
                <br><br>
            <h3>Skills</h3>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Programming Technologies:</span> &nbsp;MongoDB, ExpressJS, NodeJS, ReactJS, PyQt, SvelteKit, Figma, GIMP, Git,
                    HTML/CSS/JS, Java, Python, TypeScript, React Native, GitHub, Windows, Unix, Nginx, VPSs</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Relavant Courses:</span> &nbsp;Object-Oriented Programming (Java), Computer Science Principles (Python)</p>
                <br><br>
            <h3>Public Presentations</h3>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Georgia Technology Student Association:</span> &nbsp;Lectured on how codebases become world-changing applications.</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• TEDxGeorgiaTech:</span> &nbsp;Stand-up comedy act about computer science majors.</p>
        </div>
    </div>
</ContentTop>

<style lang="scss">
    h1 {
        color: var(--pink);
    }
    a {
        font-weight: bold;
    }
    a:hover {
        p {
            color: var(--pink);
        }
    }
    #titleSplit {
        display: flex;
        justify-content: space-between;
        width: 100%;
        .navBtn {
            border: 0.4em solid var(--pink);
            border-radius: 0.8em;
            width: 14em;
            height: min-content;
            text-align: center;
            padding: 0.8em 0em;
            transform: translateY(1.3em);
        }
        .navBtn:hover {
            background-color: var(--pink);
        }
    }
    #resume {
        margin-top: 1.5em;
        width: 100%;
        padding-bottom: 10em;
        h3 {
            margin-bottom: 0.5em;
        }
        p {
            margin-bottom: 0.15em;
            line-height: 1.3em;
        }
        .roleTitle {
            padding-left: 0.7em;
            margin-top: 0.8em;
        }
        .roleDesc {
            padding-left: 1.6em;
        }
    }

    @media all and (max-width: 740px) {
        #resume {
            width: 110%;
        }
        #titleSplit {
            flex-direction: column;
            gap: 0.6em;
            .navBtn {
                width: 80%;
                margin-bottom: 1.2em;
            }
        }

    }
    @media all and (max-width: 500px) {
        #titleSplit {
            .navBtn {
                width: 80%;
                margin: auto auto 1.2em auto
            }
        }
    }

</style>