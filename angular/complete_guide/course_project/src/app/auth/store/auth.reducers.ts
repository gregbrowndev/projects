import * as AuthActions from './auth.actions';

export interface State {
  token: string;
  authenticated: boolean;
}

const initialState: State = {
  token: null,
  authenticated: false
};

export function authReducer(state = initialState,
                            action: AuthActions.AuthActions) {
  switch (action.type) {
    // Here both cases are handled by the same code
    case AuthActions.SIGNIN:
    case AuthActions.SIGNUP:
      return {
        ...state,
        authenticated: true
      };
    case AuthActions.SIGNOUT:
      return {
        ...state,
        authenticated: false,
        token: null
      };
    case AuthActions.SET_TOKEN:
      return {
        ...state,
        token: action.payload
      };
    default:
      return state;
  }
}
