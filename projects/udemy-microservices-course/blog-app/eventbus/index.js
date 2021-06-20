const express = require("express");
const bodyParser = require("body-parser");
const axios = require("axios");

const POSTS_URL = "http://posts-clusterip-srv:4000/events";
const COMMENTS_URL = "http://comments-clusterip-srv:4001/events";
const QUERY_URL = "http://query-clusterip-srv:4002/events";
const MODERATION_URL = "http://moderation-clusterip-srv:4003/events";


const app = express();
app.use(bodyParser.json());

const events = [];

app.post("/events", (req, res) => {
    console.log("[POST: /events] ",  req.body)
    const event = req.body;

    // add event to event store
    events.push(event);

    // send event to other services
    axios.post(POSTS_URL, event);
    axios.post(COMMENTS_URL, event);
    axios.post(QUERY_URL, event);
    axios.post(MODERATION_URL, event);

    res.send({ status: "OK"});
});

app.get("/events", (req, res) => {
    console.log("[POST: /events] ",  req.body)
    res.send(events);
});

app.listen(4005, () => {
    console.log("Listening on 4005");
});
