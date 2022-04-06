import { Email, User, UserId } from '../domain/model';

/// Sign In
export type SignInCommand = {
  email: Email;
  password: string;
};

export type UserSignedIn = {
  id: UserId;
  email: Email;
  token: string;
};

export interface SignInCommandHandler {
  (command: SignInCommand): Promise<UserSignedIn>;
}

/// Sign Up
export type SignUpCommand = {
  email: Email;
  password: string;
};

export type UserSignedUp = {
  id: UserId;
  email: Email;
  token: string;
};

export interface SignUpCommandHandler {
  (command: SignUpCommand): Promise<UserSignedUp>;
}

/// Inbound Ports
export interface CoreApp {
  signIn: SignInCommandHandler;
  signUp: SignUpCommandHandler;
}

/// Outbound Ports

export interface DatabaseAdapter {
  /**
   * Adds a user to the database
   */
  addUser: (user: User) => Promise<void>;

  /**
   * Returns a user by email
   */
  getUserByEmail: (email: Email) => Promise<User | undefined>;
}

export interface UnitOfWorkContext {
  databaseAdapter: DatabaseAdapter;

  /**
   * Commits the currently active unit of work
   */
  commit: () => Promise<void>;

  /**
   * Rollback the currently active unit of work
   */
  rollback: () => Promise<void>;
}

export interface UnitOfWork {
  /**
   * Starts a unit of work
   */
  start<T>(fn: (ctx: UnitOfWorkContext) => Promise<T>): Promise<T>;
}
