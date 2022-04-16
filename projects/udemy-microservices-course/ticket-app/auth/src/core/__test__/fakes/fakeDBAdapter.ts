import { DatabaseAdapter } from '../../application/ports';
import { User, UserId } from '../../domain/user';
import { Email } from '../../domain/email';

export class FakeDBAdapter implements DatabaseAdapter {
  private readonly users: Record<string, User>;
  private userIdCounter: number;

  constructor() {
    this.users = {};
    this.userIdCounter = 0;
  }

  async nextUserId(): Promise<UserId> {
    // Create monotonically increasing UUID.
    return UserId.create(
      '00000000-0000-0000-0000-' +
        String(this.userIdCounter++).padStart(12, '0'),
    );
  }

  async addUser(user: User): Promise<void> {
    this.users[user.id.value] = user;
  }

  async getUserByEmail(email: Email): Promise<User | undefined> {
    return Object.values(this.users).find((u) => u.email.equals(email));
  }
}
