import { SignUpCommand, UserSignedUp } from '../application/ports';
import { getMongoUri } from '../../test/setup';
import { Email, JwtToken, UserId } from '../domain/model';
import { bootstrap } from '../bootstrap';

jest.setTimeout(60 * 1000);

describe('core/signUp', () => {
  it('should return UserSignedUp when correct credentials are supplied', async () => {
    // Given
    const coreApp = await bootstrap({
      DB_URL: getMongoUri(),
      JWT_KEY: 'abc',
    });

    // the current time is 1st Jan 2022 00:00:00 (the JWT token encodes the current timestamp)
    jest.useFakeTimers().setSystemTime(new Date('2020-01-01T00:00:00'));

    // When
    const signUpCommand: SignUpCommand = {
      email: Email.create('test@test.com'),
      password: 'password123',
    };
    const result = await coreApp.signUp(signUpCommand);

    jest.runOnlyPendingTimers();
    jest.useRealTimers();

    // Then
    const expected: UserSignedUp = {
      id: UserId.create('TODO'),
      email: Email.create('test@test.com'),
      token: JwtToken.create(
        'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6IjAyOGU4ZTE3LTMxYTYtNGI5OS1hYjgwLWM1NWNiODk0MDJkYSIsImVtYWlsIjoidGVzdEB0ZXN0LmNvbSIsImlhdCI6MTY0OTUwNDU2NX0.x5kagcea4G16X-2qso2jdS3ee_i1HUkjxUy0fIUHoHM',
      ),
    };
    expect(result).toEqual(expected);
  });
});
