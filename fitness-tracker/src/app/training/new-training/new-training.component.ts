import {Component, OnDestroy, OnInit} from '@angular/core';
import {FormBuilder, FormControl, Validators} from '@angular/forms';
import {Subject} from 'rxjs/Subject';
import {takeUntil} from 'rxjs/operators';

import {TrainingService} from '../training.service';
import {ExerciseModel} from '../exercise.model';
import {UIService} from '../../shared/ui.service';

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
              private fb: FormBuilder,
              private uiService: UIService) {
  }

  ngOnInit() {
    // initialise control
    this.control = this.fb.control(null, Validators.required);

    // subscribe to loading events
    this.uiService.loadingStateChanged.pipe(
      takeUntil(this.ngUnsubscribe)
    ).subscribe(loading => this.isLoading = loading);

    // React to changes in exercises
    this.trainingService.exercisesChanged.pipe(
      takeUntil(this.ngUnsubscribe)
    ).subscribe((exercises: ExerciseModel[]) => {
      this.exercises = exercises;
    });

    // Fetch exercises
    this.fetchExercises();
  }

  ngOnDestroy() {
    this.ngUnsubscribe.next();
    this.ngUnsubscribe.complete();
  }

  fetchExercises() {
    // Get data
    this.trainingService.fetchAvailableExercises();
  }

  onTrainingStart() {
    this.trainingService.startExercise(this.control.value);
  }
}
