const axios = require("axios");
const cron = require('node-cron');
const express = require("express");
const cors = require("cors");
const dotenv = require("dotenv");

dotenv.config() //load environment vars
const app = express();
app.use(express.json()) //Content-Type specficic middleware
app.use(cors()) //cors middleware

const port = process.env.PORT

let activityArray = []
app.get('/api/activity', (req, res) => {
  res.send(activityArray)
})

app.get('/api/posts', (req, res) => {
  //fetch overflow, twitter, github every 10 minutes
  //shuffle
  //save
})

app.get('/api/posts/:id', (req, res) => {
  //fetch overflow, twitter, github every 10 minutes
  //shuffle
  //save
})

app.get('/api/chat', (req, res) => {
  //run request through chatbot
  //return res
})

//use '0 */9 * * *' for every 6 hours
//use '* * * * *' every minute
cron.schedule('* * * * *', () => { //* runs every minute
  activityArray = [[], [], []]

  axios.get('https://api.stackexchange.com/2.3/users/20668816/answers?order=desc&sort=activity&site=stackoverflow&filter=!Fc6b7wC(edaHKUCJsGdLr*ztqT')
  .then(function (response) {
    activityArray[0] = (response.data.items)
  })
  .catch(function (error) {
    console.log(error);
  });

  axios.get('https://api.github.com/users/r4c3/events/public')
  .then(function (response) {
    let commitsArray = []

    response.data.forEach(event => {
      if (event.type != "PushEvent") return
      event.payload.commits.forEach(commit => {
        commitsArray.push({
          repo: event.repo.name,
          commitMessage: commit.message,
          commitUrl: commit.html_url,
          createdAt: event.created_at
        })
      })
    });

    activityArray[1] = (commitsArray.slice(0, 12))
  })
  .catch(function (error) {
    console.log(error);
  });

  axios.get('https://api.twitter.com/2/users/1595190378122059777/tweets', {
    headers: {
      Authorization: process.env.TWITTER_TOKEN
    }
  })
  .then(function (response) {
    const listToKeep = ["text"]
    const cleanedTweets = response.data.data.map(tweet => listToKeep.reduce((newTweet, key) => {
      newTweet[key] = tweet[key]
      return newTweet
    }, {}))
    activityArray[2] = (activityArray.push(cleanedTweets))
  })
  .catch(function (error) {
    console.log(error);
  });
});

app.listen(port, () => {
  console.log(`Backend is listening on port ${port}`)
})