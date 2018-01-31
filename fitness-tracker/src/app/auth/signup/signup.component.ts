import {Component, OnDestroy, OnInit, ViewChild} from '@angular/core';
import {NgForm} from '@angular/forms';
import {AuthService} from '../auth.service';
import {UIService} from '../../shared/ui.service';
import {takeUntil} from 'rxjs/operators';
import {Subject} from 'rxjs/Subject';

@Component({
  selector: 'app-signup',
  templateUrl: './signup.component.html',
  styleUrls: ['./signup.component.css']
})
export class SignupComponent implements OnInit, OnDestroy {
  private ngUnsubscribe = new Subject<void>();
  maxDate;
  isLoading = false;

  constructor(private authService: AuthService,
              private uiService: UIService) {
  }

  ngOnInit() {
    this.uiService.loadingStateChanged.pipe(
      takeUntil(this.ngUnsubscribe)
    ).subscribe(loading => this.isLoading = loading);


    this.maxDate = new Date();
    this.maxDate.setFullYear(this.maxDate.getFullYear() - 18);
  }

  ngOnDestroy() {
    this.ngUnsubscribe.next();
    this.ngUnsubscribe.complete();
  }

  onSubmit(form: NgForm) {
    this.authService.registerUser({
      email: form.value.email,
      password: form.value.password
    });
  }

}
