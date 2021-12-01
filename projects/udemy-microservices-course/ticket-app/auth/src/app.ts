import express from 'express';
import 'express-async-errors';
import { json } from 'body-parser';
import cookieSession from 'cookie-session';

import { currentUserRouter } from './routes/current-user';
import { signinRouter } from './routes/signin';
import { signoutRouter } from './routes/signout';
import { signupRouter } from './routes/signup';
import { errorHandler } from './middlewares/error-handling';
import { NotFoundError } from './errors/not-found-error';

const app = express();

app.set('trust proxy', true);
app.use(json());
app.use(
  cookieSession({
    signed: false,
    secure: process.env.NODE_ENV !== 'test',
  }),
);

app.use([currentUserRouter, signinRouter, signoutRouter, signupRouter]);

app.all('*', async (req, res, next) => {
  console.log(`Request received`);
  throw new NotFoundError();
});

app.use(errorHandler);

export { app };
