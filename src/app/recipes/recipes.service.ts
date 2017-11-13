import {Injectable} from '@angular/core';
import {Subject} from 'rxjs/Subject';

import {RecipeModel} from './recipe.model';
import {IngredientModel} from '../shared/ingredient.model';
import {Http, Response} from '@angular/http';
import 'rxjs/Rx';
import {AuthService} from '../auth/auth.service';

@Injectable()
export class RecipesService {
  recipesChanged = new Subject<RecipeModel[]>();

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
        new IngredientModel('Salad', 1)
      ])
  ];

  constructor(private http: Http,
              private authService: AuthService) {
  }

  save() {
    return this.http.put('https://ng-recipe-book-a8b74.firebaseio.com/recipes.json', this.recipes)
      .subscribe(
        (response: Response) => console.log(response)
      );
  }

  fetch() {
    return this.http.get('https://ng-recipe-book-a8b74.firebaseio.com/recipes.json')
      .map(
        (response: Response) => {
          const recipes: RecipeModel[] = response.json();
          for (let recipe of recipes) {
            if (!recipe['ingredients']) {
              console.log(recipe);
              recipe['ingredients'] = [];
            }
          }
          return recipes;
        }
      )
      .subscribe(
        (recipes: RecipeModel[]) => {
          this.recipes = recipes;
          this.emitRecipesChanged();
        }
      );
  }

  getRecipes() {
    return this.recipes.slice();
  }

  getRecipe(id: number) {
    return this.recipes[id];
  }

  addRecipe(recipe: RecipeModel) {
    this.recipes.push(recipe);
    this.emitRecipesChanged();
  }

  updateRecipe(index: number, recipe: RecipeModel) {
    this.recipes[index] = recipe;
    this.emitRecipesChanged();
  }

  emitRecipesChanged() {
    this.recipesChanged.next(this.recipes.slice());
  }

  deleteRecipe(index: number) {
    console.log('delete id: ' + index);
    this.recipes.splice(index, 1);
    this.emitRecipesChanged();
  }
}
