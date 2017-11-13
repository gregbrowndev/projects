import {Actions, Effect} from '@ngrx/effects';
import {Injectable} from '@angular/core';
import { fromPromise} from 'rxjs/observable/fromPromise';
import * as firebase from 'firebase';

import * as AuthActions from './auth.actions';
import {Observable} from 'rxjs/Observable';

@Injectable()
export class AuthEffects {

  @Effect()
  authSignup = this.actions$
    .ofType(AuthActions.TRY_SIGNUP)
    .map((action: AuthActions.TrySignup) => {
      return action.payload;
    })
    .switchMap((authData: {username: string, password: string}) => {
      return fromPromise(firebase.auth().createUserWithEmailAndPassword(authData.username, authData.password));
    })
    .switchMap(() => {
      return this.getToken();
    })
    .mergeMap((token: string) => {
      return [
        {
          type: AuthActions.SIGNUP
        },
        {
          type: AuthActions.SET_TOKEN,
          payload: token
        }
      ];
    });

  @Effect()
  authSignin = this.actions$
    .ofType(AuthActions.TRY_SIGNIN)
    .map((action: AuthActions.TrySignin) => {
      return action.payload;
    })
    .switchMap((authData: {username: string, password: string}) => {
      return fromPromise(firebase.auth().signInWithEmailAndPassword(authData.username, authData.password));
    })
    .switchMap(() => {
      return this.getToken();
    })
    .mergeMap((token: string) => {
      return [
        {
          type: AuthActions.SIGNIN
        },
        {
          type: AuthActions.SET_TOKEN,
          payload: token
        }
      ];
    });

  @Effect()
  authSignout = this.actions$
    .ofType(AuthActions.TRY_SIGNOUT)
    .switchMap(() => {
      return fromPromise(firebase.auth().signOut());
    })
    .map(() => {
      return {
        type: AuthActions.SIGNOUT
      };
    });

  private getToken(): Observable<string> {
    return fromPromise(firebase.auth().currentUser.getIdToken());
  }

  constructor(private actions$: Actions) {}
}
