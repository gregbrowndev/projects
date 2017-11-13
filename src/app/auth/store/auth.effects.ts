import {Injectable} from '@angular/core';
import {Router} from '@angular/router';
import {Actions, Effect} from '@ngrx/effects';
import { fromPromise} from 'rxjs/observable/fromPromise';
import * as firebase from 'firebase';
import {Observable} from 'rxjs/Observable';

import * as AuthActions from './auth.actions';

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
      this.navHome();
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
      this.navHome();
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

  @Effect({dispatch: false})
  authSignout = this.actions$
    .ofType(AuthActions.SIGNOUT)
    .switchMap(() => {
      return fromPromise(firebase.auth().signOut());
    })
    .do(() => {
      this.navHome();
    });

  private getToken(): Observable<string> {
    return fromPromise(firebase.auth().currentUser.getIdToken());
  }

  private navHome() {
    this.router.navigate(['/']);
  }

  constructor(private actions$: Actions,
              private router: Router) {}
}
