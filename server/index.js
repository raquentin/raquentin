const express = require("express");
const cors = require("cors");
const dotenv = require("dotenv");

dotenv.config() //load environment vars
const app = express();
app.use(express.json()) //Content-Type specficic middleware
app.use(cors()) //cors middleware
const port = process.env.PORT;

app.listen(port, () => {
  console.log(`Backend is listening on port ${port}`)
})