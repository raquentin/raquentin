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
            <a class="navBtn" download href="RaceWilliamsResume.pdf"><h2 style="transform: translateY(-0.02em);">download PDF</h2></a>
        </div>
        <div id="resume">
            <h3>Work Experience</h3>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Harris County Board of Education</span> &nbsp;Livestreaming Intern (March 2021 - June 2022)</p>
                    <p class="roleDesc">• Operated cameras, PCs, & mixers to stream county board meetings; 1/1 BoE student interns; paid in cash, tech  skills, & new political perspectives</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Sheepa</span> &nbsp;Co-founder and Web Developer (May 2021 - July 2022)</p>
                    <p class="roleDesc">• Began as a clothing company, morphed into electronics resale and sneaker store. Former clothing website now serves as a template for SvelteKit web-apps</p>
                    <p class="roleDesc">• Initial investment of $200, >$12,000 in total sales; 100% positive reviews</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• NPM Properties</span> &nbsp;App Engineer and Assitant Network Manager Intern (May 2022 - August 2022)</p>
                    <p class="roleDesc">• Remade the interface of tornado warning app TornadoFree in React Native</p>
                    <p class="roleDesc">• Analyzed the emergency weather app market to find a niche that can separate TornadoFree from competitors</p>
                    <p class="roleDesc">• Documented current Network Manager's daily tasks so future managers can more efficiently take over the job</p>
                    <p class="roleDesc">• Began shifting company services from deprecated technologies onto modern frameworks</p>
                <br><br>
            <h3>Education</h3>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Harris County High School</span> &nbsp;(August 2018 - May 2022)</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Georgia Institute of Technology</span> &nbsp;(August 2022 - present)</p>
                    <p class="roleDesc">• Gold Scholarship recipient</p>
                <br><br>
            <h3>Projects</h3>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• RaceWilliams.com</span> &nbsp;Creator, etc.</p>
                    <p class="roleDesc">• Site serves as both my software engineering portfolio and a platform to distribute my apps</p>
                    <p class="roleDesc">• Freelance works include prescription drug lookup form, installing SSL certificates, and mood-uplifting web chatbot</p>
                    <p class="roleDesc">• Open source and personal applications include AlgoTrainer (a CS technical interview question training site),  AbstainBot (an anti-addiction AI chatbot), ChemBot (a chatbot implementation of the periodic table), and more</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• STEAMBreak</span> &nbsp;Creator, etc.</p>
                    <p class="roleDesc">• Started virtual jeopardy-style Capture the Flag competition to increase Harris County School District K-12 participation in STEAM activities despite Coronavirus</p>
                    <p class="roleDesc">• Wrote >200 individual challenges across the various STEAM disciplines</p>
                    <p class="roleDesc">• Deployed customized CTFd Docker image via Digital Ocean droplet with Ubuntu and Nginx</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Harris County Technology Student Association (TSA)</span> &nbsp;First Vice President</p>
                    <p class="roleDesc">• Developed TSA-themed retro Pong game in Python with PyGame as an ice breaker for new members at club  meetings</p>
                    <p class="roleDesc">• Led development of Py1600 (SAT studying app) to win 2nd in state for Software Development</p>
                    <p class="roleDesc">• Gave Coding lecture at 2021 GA TSA Leadercon; taught a few basic Python concepts and explained parts of Py1600’s codebase</p>
                <p class="roleTitle"><span style="color: var(--pink); font-size: 1em; font-weight: bold;">• Harris County Air Force Association CyberPatriot</span> &nbsp;Batch/C++ Programmer for Delta Cyberforce</p>
                    <p class="roleDesc">• Coded security scripts, automating our way to National Gold Tier one year and then National Platinum Tier the  next, breaking school record both times</p>
                    <p class="roleDesc">• Used skills learned in CyberPatriot to create forensic technology problems for STEAMBreak (see above)</p>
                    <p class="roleDesc">• Earned Tech Caregiver certification to teach basic online security measures in ways easy to understand</p>
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
</style>