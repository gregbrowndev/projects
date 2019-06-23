import {Component, OnInit} from '@angular/core';

import {Observable} from 'rxjs/Observable';
import {Store} from '@ngrx/store';

import {IngredientModel} from '../shared/ingredient.model';

import {StartEdit} from './store/shopping-list.actions';
import {AppState} from '../store/app.reducers';

@Component({
  selector: 'app-shopping-list',
  templateUrl: './shopping-list.component.html',
  styleUrls: ['./shopping-list.component.css']
})
export class ShoppingListComponent implements OnInit {
  shoppingListState: Observable<{ingredients: IngredientModel[]}>;

  constructor(private store: Store<AppState>) { }

  ngOnInit() {
    this.shoppingListState = this.store.select('shoppingList');
  }

  onEditItem(index: number) {
    // this.shoppingListService.startedEditting.next(index);
    this.store.dispatch(new StartEdit(index));
  }
}
