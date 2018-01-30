import {Component, OnInit} from '@angular/core';
import {TrainingService} from '../training.service';
import {ExerciseModel} from '../exercise.model';
import {FormBuilder, FormControl, Validators} from '@angular/forms';

@Component({
  selector: 'app-new-training',
  templateUrl: './new-training.component.html',
  styleUrls: ['./new-training.component.css']
})
export class NewTrainingComponent implements OnInit {
  exercises: ExerciseModel[];
  control: FormControl;

  constructor(private trainingService: TrainingService,
              private fb: FormBuilder) { }

  ngOnInit() {
    this.exercises = this.trainingService.getAvailableExercises();
    this.control = this.fb.control(null, Validators.required);
  }

  onTrainingStart() {
    this.trainingService.startExercise(this.control.value);
  }
}
