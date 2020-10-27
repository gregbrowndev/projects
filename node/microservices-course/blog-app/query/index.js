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
        posts[id] = {id, title, comments: []};
    } else if (type === 'CommentCreated') {
        const {id, content, status, postId} = data;
        const post = posts[postId];
        post.comments.push({id, content, status});
    } else if (type === "CommentUpdated") {
        console.log("[POST: /events] Handling CommentUpdated event");
        console.log("[POST: /events]", posts);
        const { id, content, postId, status } = data;
        const post = posts[postId];
        const comment = post.comments.find(c => c.id === id);
        comment.status = status;
        comment.content = content;
        console.log("[POST: /events] Handled CommentUpdated event", comment)
    }
    res.send({});
});

app.listen(port, () => {
    console.log(`Listening on ${port}`);
});
