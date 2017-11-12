import { NgModule } from '@angular/core';
import {HeaderComponent} from './header/header.component';
import {HomeComponent} from './home/home.component';
import {CommonModule} from '@angular/common';
import {HttpModule} from '@angular/http';
import {SharedModule} from '../shared/shared.module';
import {AppRoutingModule} from '../app-routing.module';
import {AuthService} from '../auth/auth.service';
import {AuthGuard} from '../auth/auth.guard';
import {RecipesService} from '../recipes/recipes.service';

@NgModule({
  declarations: [
    HeaderComponent,
    HomeComponent
  ],
  imports: [
    CommonModule,
    HttpModule,
    SharedModule,
    AppRoutingModule
  ],
  exports: [
    AppRoutingModule,
    HeaderComponent
  ],
  providers: [
    AuthService,
    AuthGuard,
    RecipesService
  ],
})
export class CoreModule { }
