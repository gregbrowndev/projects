import {Component, OnDestroy, OnInit} from '@angular/core';
import {FormBuilder, FormGroup, Validators} from '@angular/forms';
import {AuthService} from '../auth.service';
import {UIService} from '../../shared/ui.service';
import {Subject} from 'rxjs/Subject';
import {takeUntil} from 'rxjs/operators';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit, OnDestroy {
  private ngUnsubscribe = new Subject<void>();
  form: FormGroup;
  isLoading = false;

  constructor(private fb: FormBuilder,
              private authService: AuthService,
              private uiService: UIService) { }

  ngOnInit() {
    this.uiService.loadingStateChanged.pipe(
      takeUntil(this.ngUnsubscribe)
    ).subscribe(loading => this.isLoading = loading);

    this.form = this.fb.group({
      'email': this.fb.control('', [Validators.required, Validators.email]),
      'password': this.fb.control('', [Validators.required, Validators.minLength(6)]),
    });
  }

  ngOnDestroy() {
    this.ngUnsubscribe.next();
    this.ngUnsubscribe.complete();
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
