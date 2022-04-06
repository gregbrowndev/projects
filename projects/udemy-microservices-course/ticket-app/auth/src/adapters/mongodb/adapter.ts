import { DatabaseAdapter } from '../../core/application/ports';
import { Email, User } from '../../core/domain/model';
import { userFromDb, userToDb } from './converters';
import { UserModel } from './models/user';
import * as mongoose from 'mongoose';

export class MongoDBAdapter implements DatabaseAdapter {
  private readonly session: mongoose.ClientSession;

  constructor(session: mongoose.ClientSession) {
    this.session = session;
  }

  async addUser(user: User): Promise<void> {
    try {
      const userDb = userToDb(user);
      await userDb.save({ session: this.session });
    } catch (err) {
      // TODO - add proper error handling
      console.log(err);
      throw err;
    }
  }

  async getUserByEmail(email: Email): Promise<User | undefined> {
    const userDb = await UserModel.findOne({ email: email.value }).session(
      this.session,
    );
    if (!userDb) {
      return;
    }
    return userFromDb(userDb);
  }
}
