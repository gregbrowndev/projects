import {EventEmitter, Injectable} from '@angular/core';

import {RecipeModel} from './recipe.model';
import {IngredientModel} from '../shared/ingredient.model';
import {ShoppingListService} from "../shopping-list/shopping-list.service";

@Injectable()
export class RecipesService {
  recipeSelected = new EventEmitter<RecipeModel>();

  // Here we declare a list of type RecipeModel
  private recipes: RecipeModel[] = [
    new RecipeModel(
      'Pizza',
      'A bumadding pizza.',
      'https://www.bbcgoodfood.com/sites/default/files/recipe-collections/collection-image/' +
      '2013/05/frying-pan-pizza-easy-recipe-collection.jpg',
      [
        new IngredientModel('Pizza Base', 1),
        new IngredientModel('Tomato', 3),
        new IngredientModel('Cheese', 9000)
      ]
    ),
    new RecipeModel('Juicy Lucy',
      'King of burgers.',
      'http://www.alt-gifts.com/wp-content/uploads/2016/09/fast-food.jpg',
      [
        new IngredientModel('Beef Burger', 1),
        new IngredientModel('Buns', 2),
        new IngredientModel('Fries', 20),
        new IngredientModel('Salad', 0)
      ])
  ];

  constructor(private shoppingListService: ShoppingListService) { }

  getRecipes() {
    return this.recipes.slice();
  }

  updateSelectedRecipe(recipe: RecipeModel) {
    this.recipeSelected.emit(recipe);
  }

  addRecipeToShoppingList(recipe: RecipeModel) {
    this.shoppingListService.addIngredients(recipe.ingredients);
  }

}
