import { UserDoc, UserModel } from './models/user';
import { Email, Password, User, UserId } from '../../core/domain/model';

export function userToDb(user: User): UserDoc {
  return new UserModel({
    _id: user.id.value,
    email: user.email.value,
    password: user.password.value,
  });
}

export function userFromDb(userDb: UserDoc): User {
  return {
    id: UserId.create(userDb._id),
    email: Email.create(userDb.email),
    password: Password.createUnsafe(userDb.password),
  };
}
