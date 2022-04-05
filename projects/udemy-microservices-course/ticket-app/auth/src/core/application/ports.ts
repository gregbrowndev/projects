import { User } from '../domain/model';

/// Sign In
export type SignInCommand = {
  email: string;
  password: string;
};

export type UserSignedIn = {
  id: string;
  email: string;
  token: string;
};

export interface SignInCommandHandler {
  (command: SignInCommand): Promise<UserSignedIn>;
}

/// Sign Up
export type SignUpCommand = {
  email: string;
  password: string;
};

export type UserSignedUp = {
  id: string;
  email: string;
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
  getUserByEmail: (email: string) => Promise<User | undefined>;
}
