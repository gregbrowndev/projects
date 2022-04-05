import { Email, Password, User, UserId } from '../../../core/domain/model';
import { MongoDBAdapter } from '../adapter';

describe('adapters/mongodb', () => {
  it('should add a user', async () => {
    // Given
    const mongoAdapter: MongoDBAdapter = new MongoDBAdapter();
    const user: User = {
      id: UserId.create('f2b3e45d-0704-4517-b660-85c733b55bd5'),
      email: Email.create('test@test.com'),
      password: Password.create('password123'),
    };

    // When
    await mongoAdapter.addUser(user);

    // Then
    const userSearch = await mongoAdapter.getUserByEmail('test@test.com');
    expect(userSearch).toEqual(user);
  });
});
