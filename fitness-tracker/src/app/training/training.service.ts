import { Injectable } from '@angular/core';
import {BehaviorSubject} from 'rxjs/BehaviorSubject';

@Injectable()
export class TrainingService {
  onGoingTraining = new BehaviorSubject<boolean>(false);

  constructor() {
  }

  startTraining() {
    this.onGoingTraining.next(true);
  }

}
