import {Action} from '@ngrx/store';
import {ExerciseModel} from './exercise.model';

export const SET_AVAILABLE_EXERCISES = '[TRAINING] Set Available Exercises';
export const SET_FINISHED_EXERCISES = '[TRAINING] Set Finished Exercises';
export const START_EXERCISE = '[TRAINING] Start Exercise';
export const STOP_EXERCISE = '[TRAINING] Cancel Exercise';

export class SetAvailableExercises implements Action {
  readonly type = SET_AVAILABLE_EXERCISES;

  constructor(public payload: ExerciseModel[]) {}
}

export class SetFinishedExercises implements Action {
  readonly type = SET_FINISHED_EXERCISES;

  constructor(public payload: ExerciseModel[]) {}
}

export class StartExercise implements Action {
  readonly type = START_EXERCISE;

  constructor(public payload: string) {}
}

export class StopExercise implements Action {
  readonly type = STOP_EXERCISE;
}

export type TrainingActions = SetAvailableExercises | SetFinishedExercises | StartExercise | StopExercise;
