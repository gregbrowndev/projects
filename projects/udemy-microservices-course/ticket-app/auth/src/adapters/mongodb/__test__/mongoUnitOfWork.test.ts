import { Email, Password, User, UserId } from '../../../core/domain/model';
import { MongoUnitOfWork } from '../mongoUnitOfWork';
import { getMongoUri } from '../../../test/setup';

describe('adapters/mongodb/MongoUnitOfWork', () => {
  beforeAll(() => {
    jest.setTimeout(60 * 1000);
  });

  it('should commit uow', async () => {
    // Given
    const dbUri = getMongoUri();
    const uow = await MongoUnitOfWork.create(dbUri);

    const user: User = {
      id: UserId.create('f2b3e45d-0704-4517-b660-85c733b55bd5'),
      email: Email.create('test@test.com'),
      password: Password.create('password123'),
    };

    // When
    await uow.start(async (ctx) => {
      await ctx.databaseAdapter.addUser(user);
      await ctx.commit();
    });

    // Then
    await uow.start(async (ctx) => {
      const query = await ctx.databaseAdapter.getUserByEmail(user.email);
      expect(query).toEqual(user);
    });
  });
});
