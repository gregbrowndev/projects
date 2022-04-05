import { DatabaseAdapter } from '../../core/application/ports';
import { User } from '../../core/domain/model';
import { userFromDb, userToDb } from './converters';
import { UserModel } from './models/user';

export class MongoDBAdapter implements DatabaseAdapter {
  async addUser(user: User): Promise<void> {
    const userDb = userToDb(user);
    await userDb.save();
    return Promise.resolve(undefined);
  }

  async getUserByEmail(email: string): Promise<User | undefined> {
    const userDb = await UserModel.findOne({ email });
    if (!userDb) {
      return;
    }
    return userFromDb(userDb);
  }
}
