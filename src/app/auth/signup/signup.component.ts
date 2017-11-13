import {Component, OnInit, ViewChild} from '@angular/core';
import {NgForm} from '@angular/forms';
import {Store} from '@ngrx/store';

import {AuthEffects} from '../store/auth.effects';
import {AppState} from '../../store/app.reducers';
import * as AuthActions from '../store/auth.actions';

@Component({
  selector: 'app-signup',
  templateUrl: './signup.component.html',
  styleUrls: ['./signup.component.css']
})
export class SignupComponent implements OnInit {
  @ViewChild('f') form: NgForm;

  constructor(private store: Store<AppState>,
    private authEffects: AuthEffects) { }

  ngOnInit() {
  }

  onSignup() {
    const email = this.form.value.email;
    const password = this.form.value.password;
    // this.authService.signupUser(email, password);
    this.store.dispatch(new AuthActions.TrySignup({username: email, password: password}));
  }
}
