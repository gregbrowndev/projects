import {Injectable} from '@angular/core';
import {ExerciseModel} from './exercise.model';
import {Subject} from 'rxjs/Subject';
import {AngularFirestore} from 'angularfire2/firestore';

@Injectable()
export class TrainingService {
  exerciseChanged = new Subject<ExerciseModel>();
  exercisesChanged = new Subject<ExerciseModel[]>();
  finishedExercisesChanged = new Subject<ExerciseModel[]>();
  private availableExercises: ExerciseModel[] = [];
  private runningExercise: ExerciseModel;

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

    // Increment exercise count in Firestore
    this.db.doc('availableExercises/' + exerciseId).update({
      lastSelected: new Date()
    });

    this.runningExercise = this.availableExercises.find(ex => ex.id === exerciseId);
    this.exerciseChanged.next({...this.runningExercise});
  }

  completeExercise() {
    this.postData({
      ...this.runningExercise,
      date: new Date(),
      state: 'completed'
    });
    this.runningExercise = null;
    this.exerciseChanged.next(null);
  }

  cancelExercise(progress) {
    this.postData({
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

  fetchFinishedExercises() {
    this.db.collection('finishedExercises')
      .valueChanges()
      .subscribe((exercises: ExerciseModel[]) => {
        this.finishedExercisesChanged.next(exercises);
      });
  }

  private postData(exercise: ExerciseModel) {
    this.db.collection('finishedExercises')
      .add(exercise);
  }
}
