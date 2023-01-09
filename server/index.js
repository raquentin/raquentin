const axios = require("axios");
const cron = require('node-cron');
const express = require("express");
const cors = require("cors");
const dotenv = require("dotenv");

dotenv.config() //load environment vars
const app = express();
app.use(express.json()) //Content-Type specficic middleware
app.use(cors()) //cors middleware
const port = process.env.PORT;

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

app.get('/api/russian', (req, res) => {
  //fetch overflow, twitter, github every 10 minutes
  //shuffle
  //save
})

//0 */9 * * *
cron.schedule('* * * * *', () => { //* runs every minute
  activityArray = []

  axios.get('https://api.stackexchange.com/2.3/users/20668816/answers?order=desc&sort=creation&site=stackoverflow&filter=!.f9fwfe3Gt00-e)jxd2e')
  .then(function (response) {
    activityArray.push(response.data.items)
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

    activityArray.push(commitsArray.slice(0, 12))
  })
  .catch(function (error) {
    console.log(error);
  });

  axios.get('https://api.twitter.com/2/users/1595190378122059777/tweets', {
    headers: {
      Authorization: `Bearer AAAAAAAAAAAAAAAAAAAAAKKPkgEAAAAAkcCTaRYt9emYSc4EKVU%2FqAcqrck%3DABzHhwdjga3AUd5zgWhIbis0GfVHpVR6bmgpJKdYxiWoVivJPh`
    }
  })
  .then(function (response) {
    const listToKeep = ["text"]
    const cleanedTweets = response.data.data.map(tweet => listToKeep.reduce((newTweet, key) => {
      newTweet[key] = tweet[key]
      return newTweet
    }, {}))
    activityArray.push(cleanedTweets)
  })
  .catch(function (error) {
    console.log(error);
  });
});

app.listen(port, () => {
  console.log(`Backend is listening on port ${port}`)
})