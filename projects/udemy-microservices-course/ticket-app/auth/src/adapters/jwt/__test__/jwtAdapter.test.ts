import { Email, Password, User, UserId } from '../../../core/domain/model';
import { JwtAdapterIml } from '../adapter';

describe('adapters/jwt', () => {
  it('should create a signed JWT', () => {
    // Given
    // the current time is 1st Jan 2022 00:00:00 (the JWT token encodes the current timestamp)
    jest.useFakeTimers().setSystemTime(new Date('2020-01-01T00:00:00'));

    // a user has signed up
    const user: User = {
      id: UserId.create('123'),
      email: Email.create('test@test.com'),
      password: Password.create('password123'),
    };

    // When
    const adapter: JwtAdapterIml = new JwtAdapterIml('jwtkey');
    const jwtToken = adapter.sign(user);

    // Then
    expect(jwtToken.value).toEqual(
      'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6IjEyMyIsImVtYWlsIjoidGVzdEB0ZXN0LmNvbSIsImlhdCI6MTU3NzgzNjgwMH0.h-eABm2f_ZQlKY8QaruWF87QiXCGheeFkzw6R4-V0u8',
    );
  });
});
