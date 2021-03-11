import express from 'express';
import {json} from 'body-parser';

import {currentUserRouter} from "./routes/current-user";
import {signinRouter} from "./routes/signin";
import {signoutRouter} from "./routes/signout";
import {signupRouter} from "./routes/signup";
import {errorHandler} from "./middlewares/error-handling";

const PORT = 3000;

const app = express();
app.use(json());

app.use([currentUserRouter, signinRouter, signoutRouter, signupRouter]);
app.use(errorHandler);

app.listen(PORT, () => {
    console.log(`Listening on port ${PORT}`);
})