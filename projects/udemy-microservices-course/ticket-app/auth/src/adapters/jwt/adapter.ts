import { JwtAdapter } from '../../core/application/ports';
import { JwtToken, User } from '../../core/domain/model';
import jwt from 'jsonwebtoken';

export class JwtAdapterIml implements JwtAdapter {
  private readonly jwtKey: string;

  constructor(jwtKey: string) {
    this.jwtKey = jwtKey;
  }

  sign(user: User): JwtToken {
    const token = jwt.sign(
      {
        id: user.id.value,
        email: user.email.value,
      },
      this.jwtKey,
    );
    return JwtToken.create(token);
  }
}
