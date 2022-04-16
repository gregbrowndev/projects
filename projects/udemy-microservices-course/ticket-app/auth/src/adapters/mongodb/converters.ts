import { UserDoc, UserModel } from './models/user';
import { User, UserId } from '../../core/domain/user';
import { Email } from '../../core/domain/email';
import { Password } from '../../core/domain/password';

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
    password: Password.create(userDb.password, true),
  };
}
