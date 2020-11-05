const express = require("express");
const bodyParser = require("body-parser");
const {randomBytes} = require("crypto");
const cors = require("cors");
const axios = require("axios");

const EVENTBUS_URL = "http://eventbus-srv:4005/events";

const app = express();
const port = 4000;

app.use(bodyParser.json());
app.use(cors());

// Not using a DB to keep this app simple
const posts = new Map();

app.get("/posts", (req, res) => {
    // Return all posts
    console.log("[GET /posts] ", req.body);
    res.send(
        [...posts.entries()].reduce((obj, [key, value]) => ({...obj, [key]: value}), {})
    );
});

app.post("/posts", async (req, res) => {
    // Create a new post
    console.log("[POST /posts] ", req.body);
    const id = randomBytes(4).toString("hex");
    const {title} = req.body;

    const post = {id, title};
    posts.set(id, post);

    // emit event to eventbus
    await axios.post(EVENTBUS_URL, {
        type: "PostCreated",
        data: post
    }).then(r => {});

    res.status(201).send(post)
});

app.post("/events", (req, res) => {
    console.log("[POST /events] ",  req.body);

    res.send({});
});

app.listen(port, () => {
    console.log(`Listening on ${port}`);
});
