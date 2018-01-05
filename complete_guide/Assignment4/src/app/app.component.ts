import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  gameEvents: number[] = [];

  onGameEvent(gameEvent: number) {
    console.log('Game Event: ' + gameEvent);
    this.gameEvents.push(gameEvent);
  }
}
