import {Component, OnInit} from '@angular/core';
import {ActivatedRoute, Params, Router} from '@angular/router';
import {Observable} from 'rxjs/Observable';
import {Store} from '@ngrx/store';

import * as ShoppingListActions from '../../shopping-list/store/shopping-list.actions';
import * as fromRecipe from '../store/recipe.reducers';
import * as RecipesActions from '../store/recipes.actions';

@Component({
  selector: 'app-recipe-detail',
  templateUrl: './recipe-detail.component.html',
  styleUrls: ['./recipe-detail.component.css']
})
export class RecipeDetailComponent implements OnInit {
  recipeState: Observable<fromRecipe.State>;
  id: number;

  constructor(private route: ActivatedRoute,
              private router: Router,
              private store: Store<fromRecipe.FeatureState>) { }

  ngOnInit() {
    this.route.params
      .subscribe(
        (params: Params) => {
          this.id = +params['id'];
          // this.recipe = this.recipesService.getRecipe(this.id);
          this.recipeState = this.store.select('recipes');
        }
      );
  }

  onSendToShoppingList() {
    this.store.select('recipes')
      .take(1)
      .subscribe(
        (recipeState: fromRecipe.State) => {
          this.store.dispatch(
            new ShoppingListActions.AddIngredients(recipeState.recipes[this.id].ingredients)
          );
        }
      );
  }

  onDeleteRecipe() {
    // this.recipesService.deleteRecipe(this.id);
    this.store.dispatch(new RecipesActions.DeleteRecipe(this.id));
    this.router.navigate(['/recipes']);
  }
}
