import express from 'express';
import { json } from 'body-parser';

const PORT = 3000;

const app = express();
app.use(json());

app.get("/api/users/currentuser", (req, res) => {
    console.log("Request received");
    res.send("Hi there!");
});


app.listen(PORT, () => {
    console.log(`Listening on port ${PORT}`);
})