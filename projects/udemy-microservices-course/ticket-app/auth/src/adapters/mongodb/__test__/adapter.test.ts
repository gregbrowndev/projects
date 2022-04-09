import { Email, Password, User, UserId } from '../../../core/domain/model';
import { MongoDBAdapter } from '../adapter';
import mongoose from 'mongoose';

describe('adapters/mongodb', () => {
  let session: mongoose.ClientSession;
  let mongoAdapter: MongoDBAdapter;

  beforeEach(async () => {
    console.log('[test] Starting InMemory MongoDB');
    session = await mongoose.startSession();
    mongoAdapter = new MongoDBAdapter(session);
  });

  it('should add a user', async () => {
    // Given
    const user: User = {
      id: UserId.create('f2b3e45d-0704-4517-b660-85c733b55bd5'),
      email: Email.create('test@test.com'),
      password: Password.create('password123'),
    };

    // When
    await mongoAdapter.addUser(user);

    // Then
    const userSearch = await mongoAdapter.getUserByEmail(user.email);
    expect(userSearch).toEqual(user);
  });
});
