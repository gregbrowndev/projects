import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'app';
  name = '';
  username = '';
  show_password = false;
  button_clicks = [];

  onButtonClick() {
    this.show_password = !this.show_password;
    this.button_clicks.push(new Date());
  }

  getColour(i: number) {
    return i >= 4 ? 'blue' : 'transparent';
  }
}
