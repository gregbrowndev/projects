import {Component, OnDestroy, OnInit} from '@angular/core';
import {RecipesService} from '../recipes.service';
import {RecipeModel} from '../recipe.model';
import {Subscription} from 'rxjs/Subscription';

@Component({
  selector: 'app-recipe-list',
  templateUrl: './recipe-list.component.html',
  styleUrls: ['./recipe-list.component.css']
})
export class RecipeListComponent implements OnInit, OnDestroy {
  recipes: RecipeModel[];
  recipesChangedSub: Subscription;

  constructor(private recipesService: RecipesService) { }

  ngOnInit() {
    this.recipes = this.recipesService.getRecipes();
    this.recipesChangedSub = this.recipesService.recipesChanged
      .subscribe(
        (recipes: RecipeModel[]) => {
          this.recipes = recipes;
        }
      );
  }

  ngOnDestroy() {
    this.recipesChangedSub.unsubscribe();
  }
}
