import {Injectable} from '@angular/core';
import {Router} from '@angular/router';
import {Subject} from 'rxjs/Subject';

import {UserModel} from './user.model';
import {AuthDataModel} from './auth-data.model';
import {AngularFireAuth} from 'angularfire2/auth';
import {TrainingService} from '../training/training.service';

@Injectable()
export class AuthService {
  private isAuth = false;
  authChange = new Subject<boolean>();

  constructor(private router: Router,
              private fireAuth: AngularFireAuth,
              private trainingService: TrainingService) {
  }

  registerUser(authData: AuthDataModel) {
    this.fireAuth.auth.createUserWithEmailAndPassword(
      authData.email,
      authData.password
    )
      .then(result => {
        this.onAuthSuccessful();
      })
      .catch(error => {
        console.log(error);
      });
  }

  login(authData: AuthDataModel) {
    this.fireAuth.auth.signInWithEmailAndPassword(
      authData.email,
      authData.password
    )
      .then(result => {
        this.onAuthSuccessful();
      })
      .catch(error => {
        console.log(error);
      });
  }

  logout() {
    this.trainingService.cancelSubscriptions();
    this.fireAuth.auth.signOut();
    this.isAuth = false;
    this.authChange.next(false);
    this.router.navigate(['/login']);
  }

  isAuthenticated() {
    return this.isAuth;
  }

  onAuthSuccessful() {
    this.isAuth = true;
    this.authChange.next(true);
    this.router.navigate(['/training']);
  }
}
