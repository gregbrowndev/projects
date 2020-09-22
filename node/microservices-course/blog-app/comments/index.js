const express = require("express");
const bodyParser = require("body-parser");
const { randomBytes } = require("crypto");
const cors = require("cors");

const app = express();
const port = 4001;

const commentsByPostId = new Map();

app.use(bodyParser.json());
app.use(cors());

app.get("/posts/:id/comments", (req, res) => {
    console.log("GET: /posts/:id/comments")
    res.send(commentsByPostId.get(req.params.id) || []);
});

app.post("/posts/:id/comments", (req, res) => {
    console.log("POST: /posts/:id/comments")
    const postId = req.params.id;
    const commentId = randomBytes(4).toString("hex");
    const { content } = req.body;
    const comment = { "id": commentId, content };

    const comments = commentsByPostId.get(postId) || [];
    comments.push(comment);
    commentsByPostId.set(postId, comments);

    res.status(201).send(comments);
});

app.listen(port, () => {
    console.log(`Listening on ${port}`);
});
