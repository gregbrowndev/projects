import {Injectable} from '@angular/core';
import {Router} from '@angular/router';
import {Subject} from 'rxjs/Subject';

import {UserModel} from './user.model';
import {AuthDataModel} from './auth-data.model';

@Injectable()
export class AuthService {
  private user: UserModel;
  authChange = new Subject<boolean>();

  constructor(private router: Router) {
  }

  registerUser(authData: AuthDataModel) {
    this.user = {
      email: authData.email,
      userId: Math.round(Math.random() * 100000).toString()
    };
    this.onAuthSuccessful();
  }

  login(authData: AuthDataModel) {
    this.user = {
      email: authData.email,
      userId: Math.round(Math.random() * 100000).toString()
    };
    this.onAuthSuccessful();
  }

  logout() {
    this.user = null;
    this.authChange.next(false);
    this.router.navigate(['/login']);
  }

  getUser() {
    return {...this.user};
  }

  isAuth() {
    return this.user != null;
  }

  onAuthSuccessful() {
    this.authChange.next(true);
    this.router.navigate(['/training']);
  }
}
