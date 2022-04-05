import { BadRequestError } from '../../../adapters/api/errors/bad-request-error';
import { DatabaseAdapter, SignInCommand, UserSignedIn } from '../ports';
import { signIn } from '../../domain/model';

export async function signInHandler(
  databaseAdapter: DatabaseAdapter,
  command: SignInCommand,
): Promise<UserSignedIn> {
  // Fetch user
  const user = await databaseAdapter.getUserByEmail(command.email);
  if (!user) {
    throw new BadRequestError('Invalid credentials');
  }

  // Try to sign in user
  const userJwt = signIn(process.env.JWT_KEY!, user, command.password);

  // TODO - handle errors

  return {
    id: user.id.value,
    email: user.email.value,
    token: userJwt,
  };
}
