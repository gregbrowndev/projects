const express = require("express");
const bodyParser = require("body-parser");
const {randomBytes} = require("crypto");

const app = express();
const port = 4000;

app.use(bodyParser.json());

// Not using a DB to keep this app simple
const posts = new Map();

app.get("/posts", (req, res) => {
    // Return all posts
    res.send(
        [...posts.entries()].reduce((obj, [key, value]) => ({...obj, [key]: value}), {})
    );
});

app.post("/posts", (req, res) => {
    // Create a new post
    const id = randomBytes(4).toString("hex");
    const {title} = req.body;

    const post = {id, title};
    posts.set(id, post);

    res.status(201).send(post)
});

app.listen(port, () => {
    console.log(`Listening on ${port}`);
});
