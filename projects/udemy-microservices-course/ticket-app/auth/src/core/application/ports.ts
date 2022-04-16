import { User, UserId } from '../domain/user';
import { Email } from '../domain/email';
import { JwtToken } from '../domain/jwtToken';
import { UnhashedPassword } from '../domain/unhashedPassword';
import { Password } from '../domain/password';

/// Sign In
export type SignInCommand = {
  email: Email;
  password: UnhashedPassword;
};

export type UserSignedIn = {
  id: UserId;
  email: Email;
  token: JwtToken;
};

export interface SignInCommandHandler {
  (command: SignInCommand): Promise<UserSignedIn>;
}

/// Sign Up
export type SignUpCommand = {
  email: Email;
  password: Password;
};

export type UserSignedUp = {
  id: UserId;
  email: Email;
  token: JwtToken;
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

export interface JwtAdapter {
  sign: (user: User) => JwtToken;
}

export interface DatabaseAdapter {
  /**
   * Returns a globally unique user id
   */
  nextUserId: () => Promise<UserId>;

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
  jwtAdapter: JwtAdapter;

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
