import {Injectable} from '@angular/core';
import {ExerciseModel} from './exercise.model';
import {Subject} from 'rxjs/Subject';

@Injectable()
export class TrainingService {
  private availableExercises: ExerciseModel[] = [
    {id: 'crunches', name: 'Crunches', duration: 20, calories: 8},
    {id: 'touch-toes', name: 'Touch Toes', duration: 180, calories: 15},
    {id: 'side-lunges', name: 'Side Lunges', duration: 120, calories: 18},
    {id: 'burpees', name: 'Burpees', duration: 60, calories: 8},
  ];
  private runningExercise: ExerciseModel;
  private exercises: ExerciseModel[] = [];
  exerciseChanged = new Subject<ExerciseModel>();

  constructor() {
  }

  getAvailableExercises() {
    return [...this.availableExercises];
  }

  startExercise(exerciseId: string) {
    this.runningExercise = this.availableExercises.find(ex => ex.id === exerciseId);
    this.exerciseChanged.next({...this.runningExercise});
  }

  completeExercise() {
    this.exercises.push({
      ...this.runningExercise,
      date: new Date(),
      state: 'completed'
    });
    this.runningExercise = null;
    this.exerciseChanged.next(null);
  }

  cancelExercise(progress) {
    const duration = this.runningExercise.duration * (progress / 100);
    this.exercises.push({
      ...this.runningExercise,
      duration: duration,
      calories: duration,
      date: new Date(),
      state: 'cancelled'
    });
    this.runningExercise = null;
    this.exerciseChanged.next(null);
  }

  getRunningExercise() {
    return {...this.runningExercise};
  }
}
