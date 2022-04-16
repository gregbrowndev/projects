import { User, UserId } from '../../../core/domain/user';
import { MongoUnitOfWork } from '../mongoUnitOfWork';
import { getMongoUri } from '../../../test/setup';
import { Email } from '../../../core/domain/email';
import { Password } from '../../../core/domain/password';

describe('adapters/mongodb/MongoUnitOfWork', () => {
  let uow: MongoUnitOfWork;

  beforeAll(async () => {
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
      id: UserId.create('d23deee7-c14a-4b46-bac5-c634a8b0928b'),
      email: Email.create('test2@test.com'),
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
      id: UserId.create('9511bc0f-6a73-4f7e-94a9-ebcb6194b9ba'),
      email: Email.create('test3@test.com'),
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
      id: UserId.create('b5ef4e0a-5686-4f59-b8b5-94211a6ee66e'),
      email: Email.create('test4@test.com'),
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
