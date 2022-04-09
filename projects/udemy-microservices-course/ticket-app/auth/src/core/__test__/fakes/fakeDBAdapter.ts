import { DatabaseAdapter } from '../../application/ports';
import { Email, User } from '../../domain/model';

export class FakeDBAdapter implements DatabaseAdapter {
  private users: Record<string, User>;

  constructor() {
    this.users = {};
  }

  async addUser(user: User): Promise<void> {
    this.users[user.id.value] = user;
  }

  async getUserByEmail(email: Email): Promise<User | undefined> {
    return Object.values(this.users).find((u) => u.email === email);
  }
}
