import { randomBytes, scryptSync } from 'crypto';
import { ValueObject } from './valueObject';
import jwt from 'jsonwebtoken';
import { BadRequestError } from '../../adapters/api/errors/bad-request-error';
import { JwtAdapter } from '../application/ports';

const crypto = require('crypto');

export class UserId extends ValueObject<string> {
  public static create(id?: string) {
    return new UserId(id || crypto.randomUUID());
  }
}

export class Email extends ValueObject<string> {
  public static create(email: string): Email {
    // TODO - add email validation
    return new Email(email);
  }
}

export class Password extends ValueObject<string> {
  public static create(password: string): Password {
    const hashedPassword = Password.toHash(password);
    return new Password(hashedPassword);
  }

  public static createUnsafe(password: string): Password {
    return new Password(password);
  }

  static toHash(password: string): string {
    const salt = randomBytes(8).toString('hex');
    const buf: Buffer = scryptSync(password, salt, 64);
    return `${buf.toString('hex')}.${salt}`;
  }

  static compare(storedPassword: Password, suppliedPassword: string): boolean {
    const [hashedPassword, salt] = storedPassword.value.split('.');
    const buf: Buffer = scryptSync(suppliedPassword, salt, 64);
    return buf.toString('hex') === hashedPassword;
  }
}

export class JwtToken extends ValueObject<string> {
  public static create(token: string): JwtToken {
    return new JwtToken(token);
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
  password: string,
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

export function createUser(email: Email, password: string): User {
  return {
    id: UserId.create(crypto.randomUUID()),
    email: email,
    password: Password.create(password),
  };
}
