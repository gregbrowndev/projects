import {
  DatabaseAdapter,
  JwtAdapter,
  UnitOfWork,
  UnitOfWorkContext,
} from '../../core/application/ports';
import mongoose from 'mongoose';
import { MongoDBAdapter } from './adapter';
import { JwtAdapterIml } from '../jwt';

export async function createConnection(
  mongoUri: string,
): Promise<mongoose.Connection> {
  /**
   * This function returns the default Mongoose connection or creates one.
   *
   * We could create completely separate connections whenever we create a UoW.
   * However, this increases the complexity significantly, as you need to use
   * the connection to dynamically create the Mongoose models. Additionally,
   * the domain model to DTO converters would also need to be passed the model
   * explicitly. If you do use multiple connections, it would probably be best
   * to define the converters as static/instance methods on the schema/model.
   */

  try {
    if (mongoose.connection.readyState !== mongoose.STATES.connected) {
      console.log('[MongoDB Adapter] Connecting to MongoDB on', mongoUri);
      await mongoose.connect(mongoUri);
      console.log('[MongoDB Adapter] Connected to MongoDB');
    } else {
      console.log('[MongoDB Adapter] Using default connection to MongoDB');
    }

    return mongoose.connection;
  } catch (err) {
    console.error('[MongoDB Adapter] Failed to connect to MongoDB');
    console.error(err);
    throw err;
  }
}

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
  private readonly connection: mongoose.Connection;
  private readonly jwtKey: string;

  private constructor(connection: mongoose.Connection, jwtKey: string) {
    this.connection = connection;
    this.jwtKey = jwtKey;
  }

  public static async create(
    mongoUri: string,
    jwtKey: string,
  ): Promise<MongoUnitOfWork> {
    console.log('[MongoDBUnitOfWork] Creating Unit of Work');
    const connection = await createConnection(mongoUri);
    return new MongoUnitOfWork(connection, jwtKey);
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
