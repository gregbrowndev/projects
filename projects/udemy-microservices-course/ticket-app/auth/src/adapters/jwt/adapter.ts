import { JwtAdapter } from '../../core/application/ports';
import { User } from '../../core/domain/user';
import jwt, { SignOptions } from 'jsonwebtoken';
import { JwtToken } from '../../core/domain/jwtToken';

export class JwtAdapterIml implements JwtAdapter {
  private readonly jwtKey: string;
  protected signOptions?: SignOptions;

  constructor(jwtKey: string, signOptions?: SignOptions) {
    this.jwtKey = jwtKey;
    this.signOptions = signOptions;
  }

  sign(user: User): JwtToken {
    const token = jwt.sign(
      {
        id: user.id.value,
        email: user.email.value,
      },
      this.jwtKey,
      this.signOptions,
    );
    return JwtToken.create(token);
  }
}
