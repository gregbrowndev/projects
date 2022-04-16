import { ValueObject } from './valueObject';
import { Password } from './password';

export class UnhashedPassword extends ValueObject<string> {
  public static create(password: string): UnhashedPassword {
    return new UnhashedPassword(password);
  }

  public toHashed(): Password {
    return Password.create(this.value);
  }
}
