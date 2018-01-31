import {Component, OnDestroy, OnInit} from '@angular/core';
import {FormBuilder, FormControl, Validators} from '@angular/forms';
import {Subject} from 'rxjs/Subject';
import {takeUntil} from 'rxjs/operators';

import {TrainingService} from '../training.service';
import {ExerciseModel} from '../exercise.model';

@Component({
  selector: 'app-new-training',
  templateUrl: './new-training.component.html',
  styleUrls: ['./new-training.component.css']
})
export class NewTrainingComponent implements OnInit, OnDestroy {
  private ngUnsubscribe = new Subject<void>();
  exercises: ExerciseModel[];
  control: FormControl;
  isLoading = true;

  constructor(private trainingService: TrainingService,
              private fb: FormBuilder) {
  }

  ngOnInit() {
    this.control = this.fb.control(null, Validators.required);
    this.trainingService.fetchAvailableExercises();
    this.trainingService.exercisesChanged.pipe(
      takeUntil(this.ngUnsubscribe)
    ).subscribe((exercises: ExerciseModel[]) => {
      this.exercises = exercises;
      this.isLoading = false;
    });
  }

  ngOnDestroy() {
    this.ngUnsubscribe.next();
    this.ngUnsubscribe.complete();
  }

  onTrainingStart() {
    this.trainingService.startExercise(this.control.value);
  }
}
