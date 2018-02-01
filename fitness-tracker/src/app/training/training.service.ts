import {Injectable} from '@angular/core';
import {AngularFirestore} from 'angularfire2/firestore';
import {Subject} from 'rxjs/Subject';
import {Subscription} from 'rxjs/Subscription';
import {Store} from '@ngrx/store';

import {ExerciseModel} from './exercise.model';
import {UIService} from '../shared/ui.service';
import * as fromTraining from '../training/training.reducer';
import * as Training from '../training/training.actions';
import * as UI from '../shared/ui.actions';

@Injectable()
export class TrainingService {
  private runningExercise: ExerciseModel;
  private fireSubs: Subscription[] = [];

  constructor(private db: AngularFirestore,
              private store: Store<fromTraining.State>,
              private uiService: UIService) {
  }

  fetchAvailableExercises() {
    this.store.dispatch(new UI.StartLoading());
    this.fireSubs.push(
      this.db.collection('availableExercises')
        .snapshotChanges()
        .map(docArray => {
          // throw (new Error);
          return docArray.map(doc => {
            return {
              id: doc.payload.doc.id,
              ...(doc.payload.doc.data() as ExerciseModel)
            };
          });
        }).subscribe(
        (exercises: ExerciseModel[]) => {
          this.store.dispatch(new Training.SetAvailableExercises(exercises));
          this.store.dispatch(new UI.StopLoading());
        },
        error => {
          this.store.dispatch(new UI.StopLoading());
          this.uiService.showSnackbar('Failed to fetch exercises, please try again!', null, 3000);
          this.store.dispatch(new Training.SetAvailableExercises([]));
        })
    );
  }

  startExercise(exerciseId: string) {
    this.store.dispatch(new Training.StartExercise(exerciseId));
  }

  completeExercise() {
    this.postData({
      ...this.runningExercise,
      date: new Date(),
      state: 'completed'
    });
    this.runningExercise = null;
    this.store.dispatch(new Training.StopExercise());
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
    this.store.dispatch(new Training.StopExercise());
  }

  getRunningExercise() {
    return {...this.runningExercise};
  }

  fetchFinishedExercises() {
    this.fireSubs.push(
      this.db.collection('finishedExercises')
        .valueChanges()
        .subscribe(
          (exercises: ExerciseModel[]) => {
            this.store.dispatch(new Training.SetFinishedExercises(exercises));
          })
    );
  }

  cancelSubscriptions() {
    this.fireSubs.forEach(sub => sub.unsubscribe());
  }

  private postData(exercise: ExerciseModel) {
    this.db.collection('finishedExercises')
      .add(exercise);
  }
}
