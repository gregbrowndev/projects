import {Action} from '@ngrx/store';

export const SIGNIN = 'SIGNIN';
export const SIGNOUT = 'SIGNOUT';
export const SIGNUP = 'SIGNUP';
export const SET_TOKEN = 'SET_TOKEN';

export class Signin implements Action {
  readonly type = SIGNIN;
}

export class Signout implements Action {
  readonly type = SIGNOUT;
}

export class Signup implements Action {
  readonly type = SIGNUP;
}

export class SetToken implements Action {
  readonly type = SET_TOKEN;

  constructor(public payload: string) {}
}

export type AuthActions = Signin
  | Signout
  | Signup
  | SetToken;
