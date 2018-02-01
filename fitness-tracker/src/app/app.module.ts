import {BrowserModule} from '@angular/platform-browser';
import {NgModule} from '@angular/core';
import {BrowserAnimationsModule} from '@angular/platform-browser/animations';
import {AngularFireModule} from 'angularfire2';
import {AngularFirestoreModule} from 'angularfire2/firestore';
import {StoreModule} from '@ngrx/store';

import {AppComponent} from './app.component';
import {HomeComponent} from './home/home.component';
import {AppRoutingModule} from './app-routing.module';
import {ReactiveFormsModule} from '@angular/forms';
import {HeaderComponent} from './navigation/header/header.component';
import {SidenavListComponent} from './navigation/sidenav-list/sidenav-list.component';
import {TrainingService} from './training/training.service';
import {AuthService} from './auth/auth.service';
import {environment} from '../environments/environment';
import {UIService} from './shared/ui.service';
import {AuthModule} from './auth/auth.module';
import {SharedModule} from './shared/shared.module';
import {reducers} from './app.reducer';


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
    AngularFireModule.initializeApp(environment.firebase),
    AngularFirestoreModule,
    ReactiveFormsModule,

    // NgRx
    StoreModule.forRoot(reducers),

    AppRoutingModule,
    SharedModule,
    AuthModule
  ],
  providers: [
    TrainingService,
    AuthService,
    UIService
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
