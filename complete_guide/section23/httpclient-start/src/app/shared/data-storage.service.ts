import { Injectable } from '@angular/core';
import { Response } from '@angular/http';
import 'rxjs/Rx';

import { RecipeService } from '../recipes/recipe.service';
import { Recipe } from '../recipes/recipe.model';
import { AuthService } from '../auth/auth.service';
import {HttpClient, HttpParams, HttpRequest} from '@angular/common/http';

@Injectable()
export class DataStorageService {
  constructor(private http: HttpClient,
              private recipeService: RecipeService,
              private authService: AuthService) {
  }

  storeRecipes() {
    const token = this.authService.getToken();

    return this.http.put('https://ng-recipe-book-a8b74.firebaseio.com//recipes.json?auth=' + token,
      this.recipeService.getRecipes(), {
      observe: 'events'
      });

    const req = new HttpRequest(
      'PUT',
      'https://ng-recipe-book-a8b74.firebaseio.com/recipes.json',
      this.recipeService.getRecipes(),
      {
        params: new HttpParams().append('auth', token),
        reportProgress: true
      });
    return this.http.request(req);
  }

  getRecipes() {
    const token = this.authService.getToken();

    this.http.get<Recipe[]>('https://ng-recipe-book-a8b74.firebaseio.com//recipes.json?auth=' + token)
      .map(
        (recipes) => {
          for (let recipe of recipes) {
            if (!recipe['ingredients']) {
              recipe['ingredients'] = [];
            }
          }
          return recipes;
        }
      )
      .subscribe(
        (recipes: Recipe[]) => {
          this.recipeService.setRecipes(recipes);
        }
      );
  }
}
