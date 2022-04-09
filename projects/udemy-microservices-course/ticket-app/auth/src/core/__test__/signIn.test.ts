import {
  SignInCommand,
  SignUpCommand,
  UserSignedIn,
  UserSignedUp,
} from '../application/ports';
import { coreApp } from '../../test/setup';
import { Email, JwtToken } from '../domain/model';

jest.setTimeout(60 * 1000);

describe('core/signIn', () => {
  it('should return UserSignedIn when correct credentials are supplied', async () => {
    // Given
    // the current time is 1st Jan 2022 00:00:00 (the JWT token encodes the current timestamp)
    jest.useFakeTimers().setSystemTime(new Date('2020-01-01T00:00:00'));

    console.log('[signin.test.ts] Core: ', coreApp);

    // AND a user has signed up
    let userSignedUp: UserSignedUp;

    try {
      const signUpCommand: SignUpCommand = {
        email: Email.create('test@test.com'),
        password: 'password123',
      };
      userSignedUp = await coreApp.signUp(signUpCommand);
    } catch (err) {
      console.error(err);
      throw err;
    }

    // When
    // the user signs in
    const signInCommand: SignInCommand = {
      email: Email.create('test@test.com'),
      password: 'password123',
    };
    const result = await coreApp.signIn(signInCommand);

    // Then
    const expected: UserSignedIn = {
      id: userSignedUp.id,
      email: Email.create('test@test.com'),
      token: JwtToken.create(
        'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6IjAyOGU4ZTE3LTMxYTYtNGI5OS1hYjgwLWM1NWNiODk0MDJkYSIsImVtYWlsIjoidGVzdEB0ZXN0LmNvbSIsImlhdCI6MTY0OTUwNDU2NX0.x5kagcea4G16X-2qso2jdS3ee_i1HUkjxUy0fIUHoHM',
      ),
    };
    expect(result).toEqual(expected);
  });
});
