import express from 'express';
import 'express-async-errors';
import {json} from 'body-parser';
import mongoose from 'mongoose';

import {currentUserRouter} from "./routes/current-user";
import {signinRouter} from "./routes/signin";
import {signoutRouter} from "./routes/signout";
import {signupRouter} from "./routes/signup";
import {errorHandler} from "./middlewares/error-handling";
import {NotFoundError} from "./errors/not-found-error";

const PORT = 3000;

const app = express();
app.use(json());

app.use([currentUserRouter, signinRouter, signoutRouter, signupRouter]);

app.all("*", async (req, res, next) => {
    throw new NotFoundError();
});

app.use(errorHandler);

const start = async () => {
    try {
        await mongoose.connect('mongodb://auth-mongo-src:27017/auth', {
            useNewUrlParser: true,
            useUnifiedTopology: true,
            useCreateIndex: true
        });
        console.log('Connected to MongoDb');
    } catch (err) {
        console.error(err);
    }

    app.listen(PORT, () => {
        console.log(`Listening on port ${PORT}`);
    })
}

start();
