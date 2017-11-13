import {Component, OnInit, ViewChild} from '@angular/core';
import {NgForm} from '@angular/forms';
import {AppState} from '../../store/app.reducers';
import {Store} from '@ngrx/store';
import * as AuthActions from '../store/auth.actions';

@Component({
  selector: 'app-signin',
  templateUrl: './signin.component.html',
  styleUrls: ['./signin.component.css']
})
export class SigninComponent implements OnInit {
  @ViewChild('f') form: NgForm;

  constructor(private store: Store<AppState>) { }

  ngOnInit() {
  }

  onSignin() {
    const email = this.form.value.email;
    const password = this.form.value.password;
    // this.authService.signinUser(email, password);
    this.store.dispatch(new AuthActions.TrySignin({username: email, password: password}));
  }
}
