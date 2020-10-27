const express = require("express");
const bodyParser = require("body-parser");
const axios = require("axios");

const app = express();
const port = 4003;

app.use(bodyParser.json());

app.post("/events", async (req, res) => {
    console.log("[POST /events]", req.body);
    const {type, data} = req.body;
    if (type === "CommentCreated") {
        console.log("[POST /events] ", type);
        const status = data.content.includes("orange") ? "rejected" : "approved";
        await axios.post("http://localhost:4005/events", {
            type: "CommentModerated",
            data: {
                ...data,
                status
            }
        });
    }

    res.send({});
});


app.listen(port, () => {
    console.log(`Listening on ${port}`);
});
