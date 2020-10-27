const express = require("express");
const bodyParser = require("body-parser");
const cors = require("cors");

const port = 4002;
const app = express();

app.use(bodyParser.json());
app.use(cors());

const posts = {};

app.get("/posts", (req, res) => {
    console.log("[GET: /posts] ", req.body)
    res.send(posts);
});

app.post("/events", (req, res) => {
    console.log("[POST: /events] ", req.body)

    const {type, data} = req.body;

    if (type === 'PostCreated') {
        const {id, title} = data;
        posts[id] = {id, title, comments: []}
    } else if (type === 'CommentCreated') {
        const {id, content, postId} = data;
        const post = posts[postId];
        post.comments.push({id, content});
    }

    res.send({});
});

app.listen(port, () => {
    console.log(`Listening on ${port}`);
});
