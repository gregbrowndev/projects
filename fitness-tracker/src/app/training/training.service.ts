import { Injectable } from '@angular/core';
import {BehaviorSubject} from 'rxjs/BehaviorSubject';

@Injectable()
export class TrainingService {
  trainingStart = new BehaviorSubject<boolean>(false);

  constructor() {
  }

  startTraining() {
    this.trainingStart.next(true);
  }

  stopTraining() {
    this.trainingStart.next(false);
  }

}
