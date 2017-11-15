import {Actions, Effect} from '@ngrx/effects';
import {HttpClient} from '@angular/common/http';
import {Injectable} from '@angular/core';
import {Store} from '@ngrx/store';

import {RecipeModel} from '../recipe.model';
import * as RecipeActions from './recipe.actions';
import * as fromRecipes from './recipe.reducers';

@Injectable()
export class RecipeEffects {

  @Effect() recipeFetch = this.actions$
    .ofType(RecipeActions.FETCH_RECIPES)
    .switchMap(
      (action: RecipeActions.FetchRecipes) => {
        return this.http.get<RecipeModel[]>('https://ng-recipe-book-a8b74.firebaseio.com/recipes.json');
      }
    )
    .map(
      (recipes: RecipeModel[]) => {
        for (const recipe of recipes) {
          if (!recipe['ingredients']) {
            recipe['ingredient'] = [];
          }
        }
        return {
          type: RecipeActions.SET_RECIPES,
          payload: recipes
        };
      }
    );

  @Effect({dispatch: false}) recipesStore = this.actions$
    .ofType(RecipeActions.STORE_RECIPES)
    .withLatestFrom(this.store.select('recipes'))
    .switchMap(([action, state]) => {
      return this.http.put<RecipeModel[]>('https://ng-recipe-book-a8b74.firebaseio.com/recipes.json', state.recipes);
    });

  constructor(private actions$: Actions,
              private http: HttpClient,
              private store: Store<fromRecipes.FeatureState>) {
  }
}
