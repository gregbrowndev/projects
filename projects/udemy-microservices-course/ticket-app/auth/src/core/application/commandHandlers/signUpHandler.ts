import { BadRequestError } from '../../../adapters/api/errors/bad-request-error';
import { SignUpCommand, UnitOfWork, UserSignedUp } from '../ports';
import { createUser, signIn } from '../../domain/model';

export async function signUpHandler(
  uow: UnitOfWork,
  command: SignUpCommand,
): Promise<UserSignedUp> {
  return uow.start(async (ctx) => {
    // Validate email
    const existingUser = await ctx.databaseAdapter.getUserByEmail(
      command.email,
    );

    // TODO - replace with domain error
    if (existingUser) {
      throw new BadRequestError('Validation error', [
        { name: 'email', reason: 'Email already exists' },
      ]);
    }

    // Create user
    console.log('Creating a user...');
    const user = createUser(command.email, command.password);

    // Persist to DB
    await ctx.databaseAdapter.addUser(user);

    // Try to sign in user
    // (most likely you would require email verification before issuing the JWT)
    const userJwt = signIn(process.env.JWT_KEY!, user, command.password);

    await ctx.commit();

    return {
      id: user.id,
      email: user.email,
      token: userJwt,
    };
  });
}
