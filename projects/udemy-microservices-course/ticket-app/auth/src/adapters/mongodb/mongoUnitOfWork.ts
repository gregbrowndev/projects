import {
  DatabaseAdapter,
  JwtAdapter,
  UnitOfWork,
  UnitOfWorkContext,
} from '../../core/application/ports';
import mongoose from 'mongoose';
import { MongoDBAdapter } from './adapter';
import { JwtAdapterIml } from '../jwt';

class MongoUnitOfWorkContext implements UnitOfWorkContext {
  private readonly session: mongoose.ClientSession;
  databaseAdapter: DatabaseAdapter;
  jwtAdapter: JwtAdapter;

  constructor(session: mongoose.ClientSession, jwtKey: string) {
    this.session = session;
    this.databaseAdapter = new MongoDBAdapter(session);
    this.jwtAdapter = new JwtAdapterIml(jwtKey);
  }

  async commit(): Promise<void> {
    await this.session.commitTransaction();
  }

  async rollback(): Promise<void> {
    await this.session.abortTransaction();
  }
}

export class MongoUnitOfWork implements UnitOfWork {
  private readonly jwtKey: string;

  private constructor(jwtKey: string) {
    this.jwtKey = jwtKey;
  }

  public static async create(
    dbUrl: string,
    jwtKey: string,
  ): Promise<MongoUnitOfWork> {
    console.log('[MongoDBUnitOfWork] Creating UoW');
    try {
      console.log('[MongoDBUnitOfWork] Connecting to MongoDB on', dbUrl);
      // TODO - refactor DB_URL into config
      await mongoose.connect(dbUrl, {
        socketTimeoutMS: 10000,
        connectTimeoutMS: 10000,
        serverSelectionTimeoutMS: 10000,
        useNewUrlParser: true,
        useUnifiedTopology: true,
        useCreateIndex: true,
        replicaSet: 'rs',
        retryWrites: false,
      });
      console.log('[MongoDBUnitOfWork] Connected to MongoDB');
    } catch (err) {
      console.error('[MongoDBUnitOfWork] Failed to connect to MongoDB');
      console.error(err);
      throw err;
    }

    return new MongoUnitOfWork(jwtKey);
  }

  async start<T>(fn: (ctx: UnitOfWorkContext) => Promise<T>): Promise<T> {
    const session = await mongoose.startSession();
    const context = new MongoUnitOfWorkContext(session, this.jwtKey);
    session.startTransaction();

    try {
      return await fn(context);
    } finally {
      if (session.inTransaction()) {
        // Rollback by default
        await context.rollback();
      }
      await session.endSession();
    }
  }
}
