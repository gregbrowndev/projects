import { BadRequestError } from '../../../adapters/api/errors/bad-request-error';
import { SignInCommand, SignInCommandHandler, UnitOfWork } from '../ports';
import { signIn } from '../../domain/model';

export function signInHandler(uow: UnitOfWork): SignInCommandHandler {
  return (command: SignInCommand) => {
    return uow.start(async (ctx) => {
      // Fetch user
      const user = await ctx.databaseAdapter.getUserByEmail(command.email);

      // TODO - replace with domain error
      if (!user) {
        throw new BadRequestError('Invalid credentials');
      }

      // Try to sign in user
      const userJwt = signIn(ctx.jwtAdapter, user, command.password);

      // TODO - handle errors

      await ctx.commit();

      return {
        id: user.id,
        email: user.email,
        token: userJwt,
      };
    });
  };
}
