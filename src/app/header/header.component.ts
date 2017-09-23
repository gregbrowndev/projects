import {Component, EventEmitter, OnInit, Output} from '@angular/core';

@Component({
  selector: 'app-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.css']
})
export class HeaderComponent implements OnInit {
  // @Output() navEvent = new EventEmitter<string>();

  constructor() { }

  ngOnInit() {
  }

  // onNavRecipes() {
  //   this.navEvent.emit('recipes');
  // }
  //
  // onNavShoppingList() {
  //   this.navEvent.emit('shopping-list');
  // }
}
