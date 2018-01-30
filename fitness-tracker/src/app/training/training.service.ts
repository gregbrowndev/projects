import { Injectable } from '@angular/core';
import {ExerciseModel} from './exercise.model';
import {Subject} from 'rxjs/Subject';

@Injectable()
export class TrainingService {
  private availableExercises: ExerciseModel[] = [
    { id: 'crunches', name: 'Crunches', duration: 30, calories: 8 },
    { id: 'touch-toes', name: 'Touch Toes', duration: 180, calories: 15 },
    { id: 'side-lunges', name: 'Side Lunges', duration: 120, calories: 18 },
    { id: 'burpees', name: 'Burpees', duration: 60, calories: 8 },
  ];
  private runningExercise: ExerciseModel;
  exerciseChanged = new Subject<ExerciseModel>();

  constructor() {
  }

  getAvailableExercises() {
    return [ ...this.availableExercises ];
  }

  startExercise(exerciseId: string) {
    this.runningExercise = this.availableExercises.find(ex => ex.id === exerciseId);
    this.exerciseChanged.next({ ...this.runningExercise });
  }

  stopExercise() {
    this.runningExercise = null;
    this.exerciseChanged.next(null);
  }

}
