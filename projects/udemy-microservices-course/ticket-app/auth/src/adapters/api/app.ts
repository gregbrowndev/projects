import express, { Express } from 'express';
import 'express-async-errors';
import { json } from 'body-parser';
import cookieSession from 'cookie-session';

import { getCurrentUserRouter } from './routes/current-user';
import { getSignOutRouter } from './routes/signout';
import { getSignUpRouter } from './routes/signup';
import { errorHandler } from './middlewares/error-handling';
import { NotFoundError } from './errors/not-found-error';
import { AppConfig, bootstrap } from '../../core/bootstrap';
import { getSignInRouter } from './routes/signin';

export async function makeApp() {
  const app = express();

  if (!process.env.JWT_KEY) {
    throw new Error("Environment variable 'JWT_KEY' must be defined");
  }

  // TODO - need to figure out if this can be passed to routes. Make sure it
  //  is thread-safe, i.e. a one instance of CoreApp is passed to each route handler
  const appConfig: AppConfig = {
    DB_URL: 'mongodb://auth-mongo-srv:27017/auth',
    JWT_KEY: process.env.JWT_KEY,
  };
  const coreApp = await bootstrap(appConfig);

  app.set('trust proxy', true);
  app.use(json());
  app.use(
    cookieSession({
      signed: false,
      secure: process.env.NODE_ENV !== 'test',
    }),
  );

  app.use([
    getCurrentUserRouter(coreApp),
    getSignInRouter(coreApp),
    getSignOutRouter(coreApp),
    getSignUpRouter(coreApp),
  ]);

  app.all('*', async (req, res, next) => {
    console.log(`Request received`);
    throw new NotFoundError();
  });

  app.use(errorHandler);

  return app;
}
