import { ValueObject } from './valueObject';

export class Email extends ValueObject<string> {
  public static create(email: string): Email {
    // TODO - add email validation
    return new Email(email);
  }
}
