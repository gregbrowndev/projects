import {Injectable} from '@angular/core';
import {ExerciseModel} from './exercise.model';
import {Subject} from 'rxjs/Subject';
import {AngularFirestore} from 'angularfire2/firestore';

@Injectable()
export class TrainingService {
  exerciseChanged = new Subject<ExerciseModel>();
  exercisesChanged = new Subject<ExerciseModel[]>();
  private availableExercises: ExerciseModel[] = [];
  private runningExercise: ExerciseModel;
  private exercises: ExerciseModel[] = [];

  constructor(private db: AngularFirestore) {
  }

  fetchAvailableExercises() {
    this.db.collection('availableExercises')
      .snapshotChanges()
      .map(docArray => {
        return docArray.map(doc => {
          return {
            id: doc.payload.doc.id,
            ...(doc.payload.doc.data() as ExerciseModel)
          };
        });
      }).subscribe((exercises: ExerciseModel[]) => {
        this.availableExercises = exercises;
        this.exercisesChanged.next([...this.availableExercises]);
    });
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
    this.exercises.push({
      ...this.runningExercise,
      duration: this.runningExercise.duration * (progress / 100),
      calories: this.runningExercise.calories * (progress / 100),
      date: new Date(),
      state: 'cancelled'
    });
    this.runningExercise = null;
    this.exerciseChanged.next(null);
  }

  getRunningExercise() {
    return {...this.runningExercise};
  }

  getFinishedExercises() {
    return this.exercises.slice();
  }
}
