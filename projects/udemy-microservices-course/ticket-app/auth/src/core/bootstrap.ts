import { CoreApp } from './application/ports';
import { signInHandler } from './application/commandHandlers/signInHandler';
import { signUpHandler } from './application/commandHandlers/signUpHandler';
import { MongoUnitOfWork } from '../adapters/mongodb/mongoUnitOfWork';

export interface AppConfig {
  DB_URL: string;
  JWT_KEY: string;
}

export async function bootstrap(appConfig: AppConfig): Promise<CoreApp> {
  const uow: MongoUnitOfWork = await MongoUnitOfWork.create(appConfig.DB_URL);

  return {
    signIn: (command) => signInHandler(uow, command),
    signUp: (command) => signUpHandler(uow, command),
  };
}
