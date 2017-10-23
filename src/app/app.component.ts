import {Component, OnInit} from '@angular/core';
import * as firebase from 'firebase';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit {

  ngOnInit() {
    firebase.initializeApp({
      apiKey: 'AIzaSyA5aam6TJQ8gPjug7LQwsnL_2u77i5UC9k',
      authDomain: 'ng-recipe-book-a8b74.firebaseapp.com'
    });
  }
}
