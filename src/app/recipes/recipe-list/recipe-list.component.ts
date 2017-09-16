import {Component, EventEmitter, OnInit, Output} from '@angular/core';
import {RecipeModel} from '../recipe.model';

@Component({
  selector: 'app-recipe-list',
  templateUrl: './recipe-list.component.html',
  styleUrls: ['./recipe-list.component.css']
})
export class RecipeListComponent implements OnInit {
  // Here we declare a list of type RecipeModel
  recipes: RecipeModel[] = [
    new RecipeModel('A Test Recipe Model', 'A simple test.',
      'https://www.bbcgoodfood.com/sites/default/files/recipe-collections/collection-image/' +
      '2013/05/frying-pan-pizza-easy-recipe-collection.jpg'),
    new RecipeModel('Another Test Recipe Model', 'Another simple test.',
      'http://www.alt-gifts.com/wp-content/uploads/2016/09/fast-food.jpg')
  ];

  @Output() recipeSelected = new EventEmitter<RecipeModel>();

  constructor() { }

  ngOnInit() {
  }

  onRecipeSelected(recipe: RecipeModel) {
    this.recipeSelected.emit(recipe);
  }
}
