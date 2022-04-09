import { BadRequestError } from '../../../adapters/api/errors/bad-request-error';
import { SignUpCommand, SignUpCommandHandler, UnitOfWork } from '../ports';
import { createUser, signIn, User } from '../../domain/model';

export function signUpHandler(uow: UnitOfWork): SignUpCommandHandler {
  return (command: SignUpCommand) => {
    return uow.start(async (ctx) => {
      // Validate email
      let existingUser: User | undefined;

      console.log('[signUpHandler] Querying for existing user');
      try {
        existingUser = await ctx.databaseAdapter.getUserByEmail(command.email);
      } catch (err) {
        console.log('[signUpHandler] Error fetching existing user');
        console.error(err);
        throw err;
      }

      console.log('[signUpHandler] Got existing user', existingUser);

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
      const userJwt = signIn(ctx.jwtAdapter, user, command.password);

      await ctx.commit();

      return {
        id: user.id,
        email: user.email,
        token: userJwt,
      };
    });
  };
}
