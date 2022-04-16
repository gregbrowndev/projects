import { ValueObject } from './valueObject';
import { BadRequestError } from '../../adapters/api/errors/bad-request-error';
import { DatabaseAdapter, JwtAdapter } from '../application/ports';
import { Email } from './email';
import { Password } from './password';
import { JwtToken } from './jwtToken';
import { UnhashedPassword } from './unhashedPassword';

const crypto = require('crypto');

export class UserId extends ValueObject<string> {
  public static create(id?: string) {
    return new UserId(id || crypto.randomUUID());
  }
}

export type User = {
  id: UserId;
  email: Email;
  password: Password;
};

export function signIn(
  jwtAdapter: JwtAdapter,
  user: User,
  password: UnhashedPassword,
): JwtToken {
  // Validate credentials
  const passwordMatch = Password.compare(user.password, password);

  // TODO - replace with domain-specific error
  if (!passwordMatch) {
    throw new BadRequestError('Invalid credentials');
  }

  // Generate JWT
  return jwtAdapter.sign(user);
}

export function signInWithoutPassword(
  jwtAdapter: JwtAdapter,
  user: User,
): JwtToken {
  return jwtAdapter.sign(user);
}

export async function createUser(
  databaseAdapter: DatabaseAdapter,
  email: Email,
  password: Password,
): Promise<User> {
  return {
    id: await databaseAdapter.nextUserId(),
    email: email,
    password: password,
  };
}
