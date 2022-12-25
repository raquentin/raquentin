const express = require("express");
const cors = require("cors");
const dotenv = require("dotenv");

dotenv.config() //load environment vars
const app = express();
app.use(express.json()) //Content-Type specficic middleware
app.use(cors()) //cors middleware
const port = process.env.PORT;

app.get('/api/activity', (req, res) => {
  //fetch overflow, twitter, github every 10 minutes
  //shuffle
  //save
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

app.listen(port, () => {
  console.log(`Backend is listening on port ${port}`)
})