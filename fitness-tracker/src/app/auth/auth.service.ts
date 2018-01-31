import {Injectable} from '@angular/core';
import {Router} from '@angular/router';
import {Subject} from 'rxjs/Subject';
import {AngularFireAuth} from 'angularfire2/auth';

import {AuthDataModel} from './auth-data.model';
import {TrainingService} from '../training/training.service';
import {UIService} from '../shared/ui.service';

@Injectable()
export class AuthService {
  private isAuth = false;
  authChange = new Subject<boolean>();

  constructor(private router: Router,
              private fireAuth: AngularFireAuth,
              private trainingService: TrainingService,
              private uiService: UIService) {
  }

  initAuthListener() {
    this.fireAuth.authState.subscribe(user => {
      if (user) {
        this.isAuth = true;
        this.authChange.next(true);
        this.router.navigate(['/training']);
      } else {
        this.trainingService.cancelSubscriptions();
        this.isAuth = false;
        this.authChange.next(false);
        this.router.navigate(['/login']);
      }
    });
  }

  registerUser(authData: AuthDataModel) {
    this.uiService.loadingStateChanged.next(true);

    this.fireAuth.auth.createUserWithEmailAndPassword(
      authData.email,
      authData.password
    ).then(result => {
      this.uiService.loadingStateChanged.next(false);
    })
      .catch(error => {
        this.uiService.loadingStateChanged.next(false);
        this.uiService.showSnackbar(error.message, null, 3000);
      });
  }

  login(authData: AuthDataModel) {
    this.uiService.loadingStateChanged.next(true);
    this.fireAuth.auth.signInWithEmailAndPassword(
      authData.email,
      authData.password
    )
      .then(result => {
        this.uiService.loadingStateChanged.next(false);
      })
      .catch(error => {
        this.uiService.loadingStateChanged.next(false);
        this.uiService.showSnackbar(error.message, null, 3000);
      });
  }

  logout() {
    this.fireAuth.auth.signOut();
  }

  isAuthenticated() {
    return this.isAuth;
  }

}
