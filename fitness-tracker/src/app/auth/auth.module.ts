import {NgModule} from '@angular/core';
import {ReactiveFormsModule} from '@angular/forms';

import {SignupComponent} from './signup/signup.component';
import {LoginComponent} from './login/login.component';
import {AngularFireAuthModule} from 'angularfire2/auth';
import {SharedModule} from '../shared/shared.module';

@NgModule({
  declarations: [
    LoginComponent,
    SignupComponent,
  ],
  imports: [
    SharedModule,
    ReactiveFormsModule,
    AngularFireAuthModule,
  ],
  exports: [

  ]
})
export class AuthModule {

}
