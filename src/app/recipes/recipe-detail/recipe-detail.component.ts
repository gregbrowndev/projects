import {Component, OnInit} from '@angular/core';
import {RecipeModel} from '../recipe.model';
import {RecipesService} from '../recipes.service';
import {ActivatedRoute, Params} from '@angular/router';

@Component({
  selector: 'app-recipe-detail',
  templateUrl: './recipe-detail.component.html',
  styleUrls: ['./recipe-detail.component.css']
})
export class RecipeDetailComponent implements OnInit {
  recipe: RecipeModel;

  constructor(private recipesService: RecipesService,
              private route: ActivatedRoute) { }

  ngOnInit() {
    this.recipe = this.recipesService.getRecipe(
      +this.route.snapshot.params['id']
    );
    this.route.params
      .subscribe(
        (params: Params) => {
          this.recipe = this.recipesService.getRecipe(+params['id']);
        }
      );
  }

  onSendToShoppingList() {
    this.recipesService.addRecipeToShoppingList(this.recipe);
  }
}
