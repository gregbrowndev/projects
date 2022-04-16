import {
  DatabaseAdapter,
  JwtAdapter,
  UnitOfWork,
  UnitOfWorkContext,
} from '../../application/ports';
import { JwtAdapterIml } from '../../../adapters/jwt';
import { FakeDBAdapter } from './fakeDBAdapter';
import { SignOptions } from 'jsonwebtoken';

class FakeUnitOfWorkContext implements UnitOfWorkContext {
  state: 'started' | 'committed' | 'rolledback';
  databaseAdapter: DatabaseAdapter;
  jwtAdapter: JwtAdapter;

  constructor(jwtKey: string) {
    this.state = 'started';
    this.databaseAdapter = new FakeDBAdapter();

    const signOptions: SignOptions = {
      // Don't include timestamp `iat` when signing JWTs as this makes the test nondeterministic. Note, we could also
      // just return a predefined static JWT
      noTimestamp: true,
    };
    this.jwtAdapter = new JwtAdapterIml(jwtKey, signOptions);
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
  context: FakeUnitOfWorkContext;

  constructor(jwtKey: string) {
    this.jwtKey = jwtKey;
    // Note: keep context as long-lived object to allow changes to persist after
    // UoW finishes. The context can be used by tester for set up and making assertions
    this.context = new FakeUnitOfWorkContext(this.jwtKey);
  }

  async start<T>(fn: (ctx: UnitOfWorkContext) => Promise<T>): Promise<T> {
    try {
      return await fn(this.context);
    } finally {
      if (this.context.inTransaction()) {
        // Rollback by default
        await this.context.rollback();
      }
    }
  }
}
