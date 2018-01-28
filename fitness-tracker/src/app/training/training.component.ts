import {Component, OnDestroy, OnInit} from '@angular/core';
import {TrainingService} from './training.service';
import {takeUntil} from 'rxjs/operators';
import {Subject} from 'rxjs/Subject';

@Component({
  selector: 'app-training',
  templateUrl: './training.component.html',
  styleUrls: ['./training.component.css']
})
export class TrainingComponent implements OnInit, OnDestroy {
  private unsubscribe = new Subject<void>();
  onGoingTraining: boolean;

  constructor(private trainingService: TrainingService) { }

  ngOnInit() {
    this.trainingService.onGoingTraining.pipe(
      takeUntil(this.unsubscribe)
    )
      .subscribe((trainingStart: boolean) => this.onGoingTraining = trainingStart);
  }

  ngOnDestroy() {
    this.unsubscribe.next();
    this.unsubscribe.complete();
  }

}
