import {Component, OnInit} from '@angular/core';
import {FormBuilder, FormControl, Validators} from '@angular/forms';
import {Store} from '@ngrx/store';
import {Observable} from 'rxjs/Observable';

import {TrainingService} from '../training.service';
import {ExerciseModel} from '../exercise.model';
import {UIService} from '../../shared/ui.service';
import * as fromRoot from '../../app.reducer';
import * as fromTraining from '../training.reducer';

@Component({
  selector: 'app-new-training',
  templateUrl: './new-training.component.html',
  styleUrls: ['./new-training.component.css']
})
export class NewTrainingComponent implements OnInit {
  exercises$: Observable<ExerciseModel[]>;
  isLoading$: Observable<boolean>;
  control: FormControl;

  constructor(private store: Store<fromTraining.State>,
              private trainingService: TrainingService,
              private fb: FormBuilder,
              private uiService: UIService) {
  }

  ngOnInit() {
    // initialise control
    this.control = this.fb.control(null, Validators.required);

    // subscribe to loading events
    this.isLoading$ = this.store.select(fromRoot.getIsLoading);

    // React to changes in exercises
    this.exercises$ = this.store.select(fromTraining.getAvailableExercises);

    // Fetch exercises
    this.fetchExercises();
  }

  fetchExercises() {
    // Get data
    this.trainingService.fetchAvailableExercises();
  }

  onTrainingStart() {
    this.trainingService.startExercise(this.control.value);
  }
}
