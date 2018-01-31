import {NgModule} from '@angular/core';
import {FlexLayoutModule} from '@angular/flex-layout';
import {FormsModule, ReactiveFormsModule} from '@angular/forms';
import {CommonModule} from '@angular/common';

import {SignupComponent} from './signup/signup.component';
import {LoginComponent} from './login/login.component';
import {MaterialModule} from '../material/material.module';
import {AngularFireAuthModule} from 'angularfire2/auth';

@NgModule({
  declarations: [
    LoginComponent,
    SignupComponent,
  ],
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    FlexLayoutModule,
    AngularFireAuthModule,
    MaterialModule,
  ],
  exports: [

  ]
})
export class AuthModule {

}
