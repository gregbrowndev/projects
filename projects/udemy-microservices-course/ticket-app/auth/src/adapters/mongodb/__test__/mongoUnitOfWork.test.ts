import { Email, Password, User, UserId } from '../../../core/domain/model';
import { MongoUnitOfWork } from '../mongoUnitOfWork';
import { getMongoUri } from '../../../test/setup';

describe('adapters/mongodb/MongoUnitOfWork', () => {
  let uow: MongoUnitOfWork;

  beforeAll(async () => {
    jest.setTimeout(60 * 1000);
    const dbUri = getMongoUri();
    const jwtKey = 'abc';
    uow = await MongoUnitOfWork.create(dbUri, jwtKey);
  });

  it('should commit uow', async () => {
    // Given
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

  it('should rollback uow', async () => {
    // Given
    const user: User = {
      id: UserId.create('f2b3e45d-0704-4517-b660-85c733b55bd5'),
      email: Email.create('test@test.com'),
      password: Password.create('password123'),
    };

    // When
    await uow.start(async (ctx) => {
      await ctx.databaseAdapter.addUser(user);
      await ctx.rollback();
    });

    // Then
    await uow.start(async (ctx) => {
      const query = await ctx.databaseAdapter.getUserByEmail(user.email);
      expect(query).toBeUndefined();
    });
  });

  // TODO - MongoDB throws error if rollback called twice. Should wrap this
  //  error
  // it('should ignore multiple rollbacks', async () => {
  //   // Given
  //   const dbUri = getMongoUri();
  //   const uow = await MongoUnitOfWork.create(dbUri);
  //
  //   // When
  //   await uow.start(async (ctx) => {
  //     await ctx.rollback();
  //     await ctx.rollback();
  //   });
  // });

  it('should rollback by default', async () => {
    // Given
    const user: User = {
      id: UserId.create('f2b3e45d-0704-4517-b660-85c733b55bd5'),
      email: Email.create('test@test.com'),
      password: Password.create('password123'),
    };

    // When
    await uow.start(async (ctx) => {
      await ctx.databaseAdapter.addUser(user);
    });

    // Then
    await uow.start(async (ctx) => {
      const query = await ctx.databaseAdapter.getUserByEmail(user.email);
      expect(query).toBeUndefined();
    });
  });

  it('should rollback on exception', async () => {
    // Given
    const user: User = {
      id: UserId.create('f2b3e45d-0704-4517-b660-85c733b55bd5'),
      email: Email.create('test@test.com'),
      password: Password.create('password123'),
    };

    // When
    try {
      await uow.start(async (ctx) => {
        await ctx.databaseAdapter.addUser(user);
        throw new Error('something went wrong!');
      });
    } catch (err) {
      // ignore
    }

    // Then
    await uow.start(async (ctx) => {
      const query = await ctx.databaseAdapter.getUserByEmail(user.email);
      expect(query).toBeUndefined();
    });
  });
});
