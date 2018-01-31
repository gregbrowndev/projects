import {BrowserModule} from '@angular/platform-browser';
import {NgModule} from '@angular/core';
import {BrowserAnimationsModule} from '@angular/platform-browser/animations';
import {FlexLayoutModule} from '@angular/flex-layout';
import {AngularFireModule} from 'angularfire2';

import {AppComponent} from './app.component';
import {MaterialModule} from './material/material.module';
import {HomeComponent} from './home/home.component';
import {AppRoutingModule} from './app-routing.module';
import {FormsModule, ReactiveFormsModule} from '@angular/forms';
import {HeaderComponent} from './navigation/header/header.component';
import {SidenavListComponent} from './navigation/sidenav-list/sidenav-list.component';
import {TrainingService} from './training/training.service';
import {AuthService} from './auth/auth.service';
import {environment} from '../environments/environment';
import {UIService} from './shared/ui.service';
import {AuthModule} from './auth/auth.module';
import {TrainingModule} from './training/training.module';


@NgModule({
  declarations: [
    AppComponent,

    HomeComponent,
    HeaderComponent,
    SidenavListComponent
  ],
  imports: [
    BrowserModule,
    BrowserAnimationsModule,
    FormsModule,
    ReactiveFormsModule,
    FlexLayoutModule,
    AngularFireModule.initializeApp(environment.firebase),
    MaterialModule,

    AppRoutingModule,
    AuthModule,
    TrainingModule
  ],
  providers: [
    TrainingService,
    AuthService,
    UIService
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
