import {Component, OnInit} from '@angular/core';
import {TrainingService} from '../training.service';

@Component({
  selector: 'app-new-training',
  templateUrl: './new-training.component.html',
  styleUrls: ['./new-training.component.css']
})
export class NewTrainingComponent implements OnInit {

  constructor(private trainingService: TrainingService) { }

  ngOnInit() {
  }

  onTrainingStart() {
    this.trainingService.startTraining();
  }
}
