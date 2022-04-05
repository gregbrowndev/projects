import { randomBytes, scryptSync } from 'crypto';
import { ValueObject } from './valueObject';
import jwt from 'jsonwebtoken';
import { BadRequestError } from '../../adapters/api/errors/bad-request-error';

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

export type User = {
  id: UserId;
  email: Email;
  password: Password;
};

function createJwt(jwtKey: string, user: User): string {
  return jwt.sign(
    {
      id: user.id.value,
      email: user.email.value,
    },
    jwtKey,
  );
}

export function signIn(jwtKey: string, user: User, password: string): string {
  // Validate credentials
  const passwordMatch = Password.compare(user.password, password);

  // TODO - replace with domain-specific error
  if (!passwordMatch) {
    throw new BadRequestError('Invalid credentials');
  }

  // Generate JWT
  return createJwt(jwtKey, user);
}

export function createUser(email: string, password: string): User {
  return {
    id: UserId.create(crypto.randomUUID()),
    email: Email.create(email),
    password: Password.create(password),
  };
}
