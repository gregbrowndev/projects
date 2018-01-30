import {Component, OnInit} from '@angular/core';
import {FormBuilder, FormControl, Validators} from '@angular/forms';
import {AngularFirestore} from 'angularfire2/firestore';
import {Observable} from 'rxjs/Observable';

import {TrainingService} from '../training.service';
import {ExerciseModel} from '../exercise.model';

@Component({
  selector: 'app-new-training',
  templateUrl: './new-training.component.html',
  styleUrls: ['./new-training.component.css']
})
export class NewTrainingComponent implements OnInit {
  exercises: Observable<any>;
  control: FormControl;

  constructor(private trainingService: TrainingService,
              private fb: FormBuilder,
              private db: AngularFirestore) { }

  ngOnInit() {
    this.control = this.fb.control(null, Validators.required);
    this.exercises = this.db.collection('availableExercises')
      .valueChanges();
  }

  onTrainingStart() {
    this.trainingService.startExercise(this.control.value);
  }
}
