import { FakeDBAdapter } from '../fakeDBAdapter';
import { User, UserId } from '../../../domain/user';
import { Email } from '../../../domain/email';
import { Password } from '../../../domain/password';

describe('fakes/fakeDBAdapter', () => {
  let adapter: FakeDBAdapter;

  beforeEach(() => {
    adapter = new FakeDBAdapter();
  });

  it('should return a user id', async () => {
    const userId1 = await adapter.nextUserId();
    expect(userId1).toEqual(
      UserId.create('00000000-0000-0000-0000-000000000000'),
    );

    const userId2 = await adapter.nextUserId();
    expect(userId2).toEqual(
      UserId.create('00000000-0000-0000-0000-000000000001'),
    );
  });

  it('should add a user', async () => {
    // Given
    const user: User = {
      id: await adapter.nextUserId(),
      email: Email.create('test@test.com'),
      password: Password.create('password123'),
    };

    // When
    await adapter.addUser(user);

    // Then
    const query = await adapter.getUserByEmail(user.email);
    expect(query).toEqual(user);
  });
});
