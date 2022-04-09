import { CoreApp } from './application/ports';
import { signInHandler } from './application/commandHandlers/signInHandler';
import { signUpHandler } from './application/commandHandlers/signUpHandler';
import { MongoUnitOfWork } from '../adapters/mongodb';

export interface AppConfig {
  DB_URL: string;
  JWT_KEY: string;
}

export async function bootstrap(appConfig: AppConfig): Promise<CoreApp> {
  console.log('[bootstrap] Bootstrapping core...');
  const uow: MongoUnitOfWork = await MongoUnitOfWork.create(
    appConfig.DB_URL,
    appConfig.JWT_KEY,
  );

  return {
    signIn: signInHandler(uow),
    signUp: signUpHandler(uow),
  };
}
