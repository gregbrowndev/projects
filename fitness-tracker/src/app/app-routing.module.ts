import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {HomeComponent} from './home/home.component';
import {SignupComponent} from './auth/signup/signup.component';
import {LoginComponent} from './auth/login/login.component';
import {TrainingComponent} from './training/training.component';
import {NewTrainingComponent} from './training/new-training/new-training.component';
import {PastTrainingComponent} from './training/past-training/past-training.component';

const routes: Routes = [
  {path: '', component: HomeComponent},
  {path: 'signup', component: SignupComponent},
  {path: 'login', component: LoginComponent},
  {path: 'training', component: TrainingComponent, children: [
      {path: '', pathMatch: 'full', redirectTo: 'new'},
      {path: 'new', component: NewTrainingComponent},
      {path: 'past', component: PastTrainingComponent},
    ]},
];

@NgModule({
  imports: [
    RouterModule.forRoot(routes),
  ],
  exports: [
    RouterModule
  ]
})
export class AppRoutingModule {
}
