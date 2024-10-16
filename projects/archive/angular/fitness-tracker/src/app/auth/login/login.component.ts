import {Component, OnInit} from '@angular/core';
import {FormBuilder, FormGroup, Validators} from '@angular/forms';
import {Store} from '@ngrx/store';
import {Observable} from 'rxjs/Observable';

import {AuthService} from '../auth.service';
import * as fromRoot from '../../app.reducer';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {
  form: FormGroup;
  isLoading$: Observable<boolean>;

  constructor(private store: Store<{ui: fromRoot.State}>,
              private fb: FormBuilder,
              private authService: AuthService) { }

  ngOnInit() {
    this.isLoading$ = this.store.select(fromRoot.getIsLoading);
    this.form = this.fb.group({
      'email': this.fb.control('', [Validators.required, Validators.email]),
      'password': this.fb.control('', [Validators.required, Validators.minLength(6)]),
    });
  }


  onSubmit() {
    this.authService.login({
      email: this.form.value.email,
      password: this.form.value.password
    });
  }

  getErrorMessage(control: string) {
    if (control === 'email') {
      const emailControl = this.form.get('email');
      if (emailControl.hasError('required')) {
        return 'Email is required';
      } else {
        return 'Email is invalid';
      }
    }

    if (control === 'password') {
      const passwordControl = this.form.get('password');
      if (passwordControl.hasError('required')) {
        return 'Password is required';
      } else if (passwordControl.hasError('minlength')) {
        return 'Password must be at least 6 characters long';
      } else {
        return 'Password is invalid';
      }
    }
  }
}
