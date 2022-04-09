import {
  DatabaseAdapter,
  JwtAdapter,
  UnitOfWork,
  UnitOfWorkContext,
} from '../../application/ports';
import { JwtAdapterIml } from '../../../adapters/jwt';
import { FakeDBAdapter } from './fakeDBAdapter';

class FakeUnitOfWorkContext implements UnitOfWorkContext {
  state: 'started' | 'committed' | 'rolledback';
  databaseAdapter: DatabaseAdapter;
  jwtAdapter: JwtAdapter;

  constructor(jwtKey: string) {
    this.state = 'started';
    this.databaseAdapter = new FakeDBAdapter();
    this.jwtAdapter = new JwtAdapterIml(jwtKey);
  }

  async commit(): Promise<void> {
    this.state = 'committed';
  }

  async rollback(): Promise<void> {
    this.state = 'rolledback';
  }

  inTransaction(): boolean {
    return this.state === 'started';
  }
}

export class FakeUnitOfWork implements UnitOfWork {
  private readonly jwtKey: string;
  history: FakeUnitOfWorkContext[];

  constructor(jwtKey: string) {
    this.jwtKey = jwtKey;
    this.history = [];
  }

  async start<T>(fn: (ctx: UnitOfWorkContext) => Promise<T>): Promise<T> {
    const context = new FakeUnitOfWorkContext(this.jwtKey);

    try {
      return await fn(context);
    } finally {
      if (context.inTransaction()) {
        // Rollback by default
        await context.rollback();
      }
      this.history.push(context);
    }
  }
}
