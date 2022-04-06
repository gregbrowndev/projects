import {
  DatabaseAdapter,
  UnitOfWork,
  UnitOfWorkContext,
} from '../../core/application/ports';
import mongoose from 'mongoose';
import { MongoDBAdapter } from './adapter';

class MongoUnitOfWorkContext implements UnitOfWorkContext {
  private readonly session: mongoose.ClientSession;
  databaseAdapter: DatabaseAdapter;

  constructor(session: mongoose.ClientSession) {
    this.session = session;
    this.databaseAdapter = new MongoDBAdapter(session);
  }

  async commit(): Promise<void> {
    await this.session.commitTransaction();
  }

  async rollback(): Promise<void> {
    await this.session.abortTransaction();
  }
}

export class MongoUnitOfWork implements UnitOfWork {
  private constructor() {}

  public static async create(dbUrl: string): Promise<MongoUnitOfWork> {
    try {
      // TODO - refactor DB_URL into config
      await mongoose.connect(dbUrl, {
        useNewUrlParser: true,
        useUnifiedTopology: true,
        useCreateIndex: true,
        replicaSet: 'rs',
        retryWrites: false,
      });
      console.log('Connected to MongoDb');
    } catch (err) {
      console.error(err);
      throw err;
    }

    return new MongoUnitOfWork();
  }

  async start<T>(fn: (ctx: UnitOfWorkContext) => Promise<T>): Promise<T> {
    const session = await mongoose.startSession();
    const context = new MongoUnitOfWorkContext(session);
    session.startTransaction();

    try {
      const result = await fn(context);
      return result;
    } finally {
      if (session.inTransaction()) {
        // Rollback by default
        await context.rollback();
      }
      // await session.endSession();
    }
  }
}
