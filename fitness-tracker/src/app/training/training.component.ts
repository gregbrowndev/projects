import {Component, OnDestroy, OnInit} from '@angular/core';
import {TrainingService} from './training.service';
import {takeUntil} from 'rxjs/operators';
import {Subject} from 'rxjs/Subject';
import {ExerciseModel} from './exercise.model';

@Component({
  selector: 'app-training',
  templateUrl: './training.component.html',
  styleUrls: ['./training.component.css']
})
export class TrainingComponent implements OnInit, OnDestroy {
  private unsubscribe = new Subject<void>();
  onGoingTraining = false;

  constructor(private trainingService: TrainingService) { }

  ngOnInit() {
    this.trainingService.exerciseChanged.pipe(
      takeUntil(this.unsubscribe)
    ).subscribe((exercise: ExerciseModel) => {
      this.onGoingTraining = exercise != null;
    });
  }

  ngOnDestroy() {
    this.unsubscribe.next();
    this.unsubscribe.complete();
  }

}
