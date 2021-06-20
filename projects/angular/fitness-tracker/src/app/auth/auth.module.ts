import {NgModule} from '@angular/core';
import {ReactiveFormsModule} from '@angular/forms';

import {SignupComponent} from './signup/signup.component';
import {LoginComponent} from './login/login.component';
import {AngularFireAuthModule} from 'angularfire2/auth';
import {SharedModule} from '../shared/shared.module';
import {AuthRoutingModule} from './auth-routing.module';

@NgModule({
  declarations: [
    LoginComponent,
    SignupComponent,
  ],
  imports: [
    SharedModule,
    ReactiveFormsModule,
    AngularFireAuthModule,
    AuthRoutingModule,
  ]
})
export class AuthModule {

}
