import {Component, EventEmitter, OnInit, Output} from '@angular/core';

@Component({
  selector: 'app-game-control',
  templateUrl: './game-control.component.html',
  styleUrls: ['./game-control.component.css']
})
export class GameControlComponent implements OnInit {
  gameEventRef;
  gameEventCount = 0;
  @Output() gameEvent = new EventEmitter<number>();

  constructor() { }

  ngOnInit() {
  }

  onGameStart() {
    // Here we create a callback function that gets called every second. We use ES6 arrow function syntax so that 'this'
    // points to the GameControlComponent instance. We also increment a counter
    if (this.gameEventRef) {
      return;
    }

    this.gameEventRef = setInterval(() => {
      this.gameEvent.emit(this.gameEventCount);
      ++this.gameEventCount;
    }, 1000 );

    console.log('Ref: ');
    console.log(this.gameEventRef);
  }

  onGameStop() {
    clearInterval(this.gameEventRef);
    this.gameEventRef = null;
  }
}
