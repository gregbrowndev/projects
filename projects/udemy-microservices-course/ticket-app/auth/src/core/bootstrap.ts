import { CoreApp } from './application/ports';
import { signInHandler } from './application/commandHandlers/signInHandler';
import { MongoDBAdapter } from '../adapters/mongodb/adapter';
import { signUpHandler } from './application/commandHandlers/signUpHandler';
import mongoose from 'mongoose';

export async function bootstrap(): Promise<CoreApp> {
  if (!process.env.JWT_KEY) {
    throw new Error("Environment variable 'JWT_KEY' must be defined");
  }

  try {
    await mongoose.connect('mongodb://auth-mongo-srv:27017/auth', {
      useNewUrlParser: true,
      useUnifiedTopology: true,
      useCreateIndex: true,
    });
    console.log('Connected to MongoDb');
  } catch (err) {
    console.error(err);
    throw err;
  }

  const mongoDbAdapter: MongoDBAdapter = new MongoDBAdapter();

  return {
    signIn: (command) => signInHandler(mongoDbAdapter, command),
    signUp: (command) => signUpHandler(mongoDbAdapter, command),
  };
}
