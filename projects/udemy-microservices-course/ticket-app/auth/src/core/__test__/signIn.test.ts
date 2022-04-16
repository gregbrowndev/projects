import {
  CoreApp,
  SignInCommand,
  SignUpCommand,
  UserSignedIn,
  UserSignedUp,
} from '../application/ports';
import { AppConfig, bootstrap } from '../bootstrap';
import { FakeUnitOfWork } from './fakes/fakeUnitOfWork';
import { Email } from '../domain/email';
import { JwtToken } from '../domain/jwtToken';
import { Password } from '../domain/password';
import { UnhashedPassword } from '../domain/unhashedPassword';

describe('core/signIn', () => {
  let coreApp: CoreApp;

  beforeEach(async () => {
    const appConfig: AppConfig = {
      DB_URL: 'mongodb://',
      JWT_KEY: 'abc',
    };
    const uow = new FakeUnitOfWork(appConfig.JWT_KEY);
    coreApp = await bootstrap(appConfig, uow);
  });

  it('should return UserSignedIn when correct credentials are supplied', async () => {
    // Given
    // TODO - should be able to control time. Note: cannot use Jest's fake timers because it causes
    // the Mongoose code in setup.ts to hang indefinitely.

    // the current time is 1st Jan 2022 00:00:00 (the JWT token encodes the current timestamp)
    // jest.setSystemTime(new Date('2020-01-01T00:00:00'));

    // AND a user has signed up
    const signUpCommand: SignUpCommand = {
      email: Email.create('test@test.com'),
      password: Password.create('password123'),
    };
    const userSignedUp = await coreApp.signUp(signUpCommand);

    // When
    // the user signs in
    const signInCommand: SignInCommand = {
      email: Email.create('test@test.com'),
      password: UnhashedPassword.create('password123'),
    };
    const result = await coreApp.signIn(signInCommand);

    // Then
    const expected: UserSignedIn = {
      id: userSignedUp.id,
      email: Email.create('test@test.com'),
      token: JwtToken.create(
        'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6IjAwMDAwMDAwLTAwMDAtMDAwMC0wMDAwLTAwMDAwMDAwMDAwMCIsImVtYWlsIjoidGVzdEB0ZXN0LmNvbSJ9.3cyZK57mzI1j1IX58Jy3IW1H_nwvgFZzzEwQBX7Mb08',
      ),
    };
    expect(result).toEqual(expected);
  });
});
