import { ValueObject } from './valueObject';

export class JwtToken extends ValueObject<string> {
  public static create(token: string): JwtToken {
    return new JwtToken(token);
  }
}
