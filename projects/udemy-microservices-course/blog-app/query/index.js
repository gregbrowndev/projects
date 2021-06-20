const express = require("express");
const bodyParser = require("body-parser");
const cors = require("cors");
const axios = require("axios");

const EVENTBUS_URL = "http://eventbus-srv:4005/events";

const port = 4002;
const app = express();

app.use(bodyParser.json());
app.use(cors());

const posts = {};

const handleEvent = (event) => {
    const {type, data} = event;
    console.log("[handleEvent] ", event);

    if (type === 'PostCreated') {
        const {id, title} = data;
        posts[id] = {id, title, comments: []};
    } else if (type === 'CommentCreated') {
        const {id, content, status, postId} = data;
        const post = posts[postId];
        post.comments.push({id, content, status});
    } else if (type === "CommentUpdated") {
        console.log("[handleEvent]", posts);
        const { id, content, postId, status } = data;
        const post = posts[postId];
        const comment = post.comments.find(c => c.id === id);
        comment.status = status;
        comment.content = content;
        console.log("[handleEvent] Handled CommentUpdated event", comment)
    } else {
        console.warn("[handleEvent] event unhandled", event);
    }
};

app.get("/posts", (req, res) => {
    console.log("[GET: /posts] ", req.body)
    res.send(posts);
});

app.post("/events", (req, res) => {
    console.log("[POST: /events] ", req.body)
    handleEvent(req.body);
    res.send({});
});

app.listen(port, async () => {
    console.log(`Listening on ${port}`);
    console.log("[Loading events]");
    const res = await axios.get(EVENTBUS_URL);
    for (let event of res.data) {
        handleEvent(event);
    }
});
