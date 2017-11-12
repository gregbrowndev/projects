import { Injectable } from '@angular/core';
import * as firebase from 'firebase';
import {Store} from '@ngrx/store';
import {Router} from '@angular/router';

import * as fromAuth from './store/auth.reducers';
import {AppState} from '../store/app.reducers';
import * as AuthActions from './store/auth.actions';
import {Observable} from 'rxjs/Observable';


@Injectable()
export class AuthService {
  token: string;

  constructor(private router: Router,
              private store: Store<AppState>) { }

  signupUser(email: string, password: string) {
    firebase.auth().createUserWithEmailAndPassword(email, password)
      .then(
        user => {
          this.store.dispatch(new AuthActions.Signup());
        }
      )
      .catch(
        error => console.log(error)
      );
  }

  signinUser(email: string, password: string) {
    firebase.auth().signInWithEmailAndPassword(email, password)
      .then(
        response => {
          this.store.dispatch(new AuthActions.Signin());
          this.getToken();
          this.router.navigate(['/']);
        }
      )
      .catch(
        error => console.log(error)
      );
  }

  logout() {
    firebase.auth().signOut();
    this.store.dispatch(new AuthActions.Signout());
    this.router.navigate(['/']);
  }

  private getToken() {
    firebase.auth().currentUser.getToken()
      .then(
        (token: string) => {
          this.store.dispatch(new AuthActions.SetToken(token));
        }
      );
  }

  isAuthenticated(): Observable<boolean> {
    return this.store.select('auth')
      .map((authState: fromAuth.State) => {
        return authState.authenticated;
      });
  }
}
