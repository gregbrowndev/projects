import { User, UserId } from '../../../core/domain/user';
import { MongoDBAdapter } from '../adapter';
import mongoose from 'mongoose';
import { Email } from '../../../core/domain/email';
import { Password } from '../../../core/domain/password';

describe('adapters/mongodb', () => {
  let session: mongoose.ClientSession;
  let mongoAdapter: MongoDBAdapter;

  beforeAll(async () => {
    console.log('[test] Starting InMemory MongoDB');
    session = await mongoose.startSession();
    mongoAdapter = new MongoDBAdapter(session);
  });

  beforeEach(async () => {
    session.startTransaction();
  });

  afterEach(async () => {
    await session.abortTransaction();
  });

  afterAll(async () => {
    await session.endSession();
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
    const result = await mongoAdapter.getUserByEmail(user.email);
    expect(result).toEqual(user);
  });
});
