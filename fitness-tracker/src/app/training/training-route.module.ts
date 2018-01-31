import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {TrainingComponent} from './training.component';

const routes: Routes = [
  {path: '', component: TrainingComponent
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
