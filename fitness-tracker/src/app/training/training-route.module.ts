import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {TrainingComponent} from './training.component';
import {AuthGuard} from '../auth/auth.guard';

const routes: Routes = [
  {path: '', component: TrainingComponent, canActivate: [AuthGuard]
    // children: [
    //   {path: '', pathMatch: 'full', component: CurrentTrainingComponent},
    //   {path: 'new', component: NewTrainingComponent},
    //   {path: 'past', component: PastTrainingComponent},
    // ]
  }
];

@NgModule({
  imports: [
    RouterModule.forChild(routes),
  ],
  exports: [
    RouterModule
  ]
})
export class TrainingRouteModule {}
