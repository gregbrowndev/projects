import { BadRequestError } from '../../../adapters/api/errors/bad-request-error';
import { SignInCommand, UnitOfWork, UserSignedIn } from '../ports';
import { signIn } from '../../domain/model';

export async function signInHandler(
  uow: UnitOfWork,
  command: SignInCommand,
): Promise<UserSignedIn> {
  return uow.start(async (ctx) => {
    // Fetch user
    const user = await ctx.databaseAdapter.getUserByEmail(command.email);

    // TODO - replace with domain error
    if (!user) {
      throw new BadRequestError('Invalid credentials');
    }

    // TODO - replace process.env with ConfigAdapter
    // Try to sign in user
    const userJwt = signIn(process.env.JWT_KEY!, user, command.password);

    // TODO - handle errors

    await ctx.commit();

    return {
      id: user.id,
      email: user.email,
      token: userJwt,
    };
  });
}
