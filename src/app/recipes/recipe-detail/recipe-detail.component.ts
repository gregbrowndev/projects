import {Component, Input, OnInit} from '@angular/core';
import {RecipeModel} from '../recipe.model';
import {RecipesService} from '../recipes.service';

@Component({
  selector: 'app-recipe-detail',
  templateUrl: './recipe-detail.component.html',
  styleUrls: ['./recipe-detail.component.css']
})
export class RecipeDetailComponent implements OnInit {
  @Input() recipe: RecipeModel;

  constructor(private recipesService: RecipesService) { }

  ngOnInit() {
  }

  onSendToShoppingList() {
    this.recipesService.addRecipeToShoppingList(this.recipe);
  }
}
