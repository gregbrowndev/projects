import 'express-async-errors';
import { makeApp } from './app';

const PORT = 3000;

const start = async () => {
  const app = await makeApp();
  app.listen(PORT, () => {
    console.log(`Listening on port ${PORT}`);
  });
};

start();
