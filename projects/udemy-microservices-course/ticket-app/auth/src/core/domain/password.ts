import { ValueObject } from './valueObject';
import { randomBytes, scryptSync } from 'crypto';
import { UnhashedPassword } from './unhashedPassword';

export class Password extends ValueObject<string> {
  public static create(password: string, hashed: boolean = false): Password {
    // TODO - add password validation
    const hashedPassword = hashed ? password : Password.toHash(password);
    return new Password(hashedPassword);
  }

  static toHash(password: string): string {
    const salt = randomBytes(8).toString('hex');
    const buf: Buffer = scryptSync(password, salt, 64);
    return `${buf.toString('hex')}.${salt}`;
  }

  static compare(
    storedPassword: Password,
    suppliedPassword: UnhashedPassword,
  ): boolean {
    const [hashedPassword, salt] = storedPassword.value.split('.');
    const buf: Buffer = scryptSync(suppliedPassword.value, salt, 64);
    return buf.toString('hex') === hashedPassword;
  }
}
