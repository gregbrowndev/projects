import { BadRequestError } from '../../../adapters/api/errors/bad-request-error';
import { DatabaseAdapter, SignUpCommand, UserSignedUp } from '../ports';
import { createUser, signIn } from '../../domain/model';

export async function signUpHandler(
  databaseAdapter: DatabaseAdapter,
  command: SignUpCommand,
): Promise<UserSignedUp> {
  // Validate email
  const existingUser = await databaseAdapter.getUserByEmail(command.email);
  if (existingUser) {
    throw new BadRequestError('Validation error', [
      { name: 'email', reason: 'Email already exists' },
    ]);
  }

  // Create user
  console.log('Creating a user...');
  const user = createUser(command.email, command.password);

  // Persist to DB
  await databaseAdapter.addUser(user);

  // Try to sign in user
  // (most likely you would require email verification before issuing the JWT)
  const userJwt = signIn(process.env.JWT_KEY!, user, command.password);

  return {
    id: user.id.value,
    email: user.email.value,
    token: userJwt,
  };
}
