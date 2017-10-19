import {Component, OnInit} from '@angular/core';
import {Response} from '@angular/http';
import 'rxjs/Rx';
import {RecipesService} from '../recipes/recipes.service';
import {ShoppingListService} from '../shopping-list/shopping-list.service';

@Component({
  selector: 'app-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.css']
})
export class HeaderComponent implements OnInit {

  constructor(private recipesService: RecipesService,
              private shoppingListService: ShoppingListService) { }

  ngOnInit() {
  }

  onSave() {
    this.recipesService.save();
    // this.shoppingListService.save();
  }

  onFetch() {
    this.recipesService.fetch();
    // this.shoppingListService.fetch();
  }
}
