import {Injectable} from '@angular/core';
import {AngularFirestore} from 'angularfire2/firestore';
import {Subscription} from 'rxjs/Subscription';
import {Store} from '@ngrx/store';

import {ExerciseModel} from './exercise.model';
import {UIService} from '../shared/ui.service';
import * as fromTraining from '../training/training.reducer';
import * as Training from '../training/training.actions';
import * as UI from '../shared/ui.actions';
import {take} from 'rxjs/operators';

@Injectable()
export class TrainingService {
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
    this.store.select(fromTraining.getActiveExercise).pipe(
      take(1)
    ).subscribe(activeExercise => {
      this.postData({
        ...activeExercise,
        date: new Date(),
        state: 'completed'
      });
      this.store.dispatch(new Training.StopExercise());
    });
  }

  cancelExercise(progress) {
    this.store.select(fromTraining.getActiveExercise).pipe(
      take(1)
    ).subscribe(activeExercise => {
      this.postData({
        ...activeExercise,
        duration: activeExercise.duration * (progress / 100),
        calories: activeExercise.calories * (progress / 100),
        date: new Date(),
        state: 'cancelled'
      });
      this.store.dispatch(new Training.StopExercise());
    });
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
