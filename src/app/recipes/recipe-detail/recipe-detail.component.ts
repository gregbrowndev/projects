import {Component, OnInit} from '@angular/core';
import {RecipeModel} from '../recipe.model';
import {RecipesService} from '../recipes.service';
import {ActivatedRoute, Params, Router} from '@angular/router';
import {Store} from '@ngrx/store';
import * as ShoppingListActions from '../../shopping-list/store/shopping-list.actions';
import {IngredientModel} from '../../shared/ingredient.model';

@Component({
  selector: 'app-recipe-detail',
  templateUrl: './recipe-detail.component.html',
  styleUrls: ['./recipe-detail.component.css']
})
export class RecipeDetailComponent implements OnInit {
  recipe: RecipeModel;
  id: number;

  constructor(private recipesService: RecipesService,
              private route: ActivatedRoute,
              private router: Router,
              private store: Store<{
                shoppingList: { ingredients: IngredientModel[] }
              }>) { }

  ngOnInit() {
    this.route.params
      .subscribe(
        (params: Params) => {
          this.id = +params['id'];
          this.recipe = this.recipesService.getRecipe(this.id);
        }
      );
  }

  onSendToShoppingList() {
    this.store.dispatch(new ShoppingListActions.AddIngredients(this.recipe.ingredients));
  }

  onDeleteRecipe() {
    this.recipesService.deleteRecipe(this.id);
    this.router.navigate(['/recipes']);
  }
}
