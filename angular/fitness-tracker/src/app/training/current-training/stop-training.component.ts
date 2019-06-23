import {Component, Inject} from '@angular/core';
import {MAT_DIALOG_DATA} from '@angular/material';

@Component({
  template: `
  <h1 mat-dialog-title>Are you sure?</h1>
  <mat-dialog-content>
    <p>You already got {{progress}}%</p>
  </mat-dialog-content>
    <mat-dialog-actions>
      <button mat-button [mat-dialog-close]="true">Yes</button>
      <button mat-button [mat-dialog-close]="false">Cancel</button>
    </mat-dialog-actions>
  `
})
export class StopTrainingComponent {
  progress: number;

  constructor(@Inject(MAT_DIALOG_DATA) private data: {progress: number}) {
    this.progress = data.progress;
  }
}
