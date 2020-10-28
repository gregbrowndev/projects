const express = require("express");
const bodyParser = require("body-parser");
const axios = require("axios");

const app = express();
app.use(bodyParser.json());

const events = [];

app.post("/events", (req, res) => {
    console.log("[POST: /events] ",  req.body)
    const event = req.body;

    // add event to event store
    events.push(event);

    // send event to other services
    axios.post("http://localhost:4000/events", event);
    axios.post("http://localhost:4001/events", event);
    axios.post("http://localhost:4002/events", event);
    axios.post("http://localhost:4003/events", event);

    res.send({ status: "OK"});
});

app.get("/events", (req, res) => {
    console.log("[POST: /events] ",  req.body)
    res.send(events);
});

app.listen(4005, () => {
    console.log("Listening on 4005");
});
