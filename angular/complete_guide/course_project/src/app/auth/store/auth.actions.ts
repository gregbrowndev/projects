import {Action} from '@ngrx/store';

export const TRY_SIGNUP = 'TRY_SIGNUP';
export const SIGNUP = 'SIGNUP';
export const TRY_SIGNIN = 'TRY_SIGNIN';
export const SIGNIN = 'SIGNIN';
export const SIGNOUT = 'SIGNOUT';
export const SET_TOKEN = 'SET_TOKEN';

export class TrySignup implements Action {
  readonly type = TRY_SIGNUP;

  constructor(public payload: {username: string, password: string}) {}
}

export class Signup implements Action {
  readonly type = SIGNUP;
}

export class TrySignin implements Action {
  readonly type = TRY_SIGNIN;

  constructor(public payload: {username: string, password: string}) {}
}

export class Signin implements Action {
  readonly type = SIGNIN;
}

export class Signout implements Action {
  readonly type = SIGNOUT;
}

export class SetToken implements Action {
  readonly type = SET_TOKEN;

  constructor(public payload: string) {}
}

export type AuthActions = Signin
  | Signout
  | Signup
  | SetToken
  | TrySignup
  | TrySignin;
