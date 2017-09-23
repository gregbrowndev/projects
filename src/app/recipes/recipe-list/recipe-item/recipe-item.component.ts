import {Component, Input, OnInit } from '@angular/core';
import {RecipeModel} from '../../recipe.model';
import {RecipesService} from '../../recipes.service';

@Component({
  selector: 'app-recipe-item',
  templateUrl: './recipe-item.component.html',
  styleUrls: ['./recipe-item.component.css']
})
export class RecipeListItemComponent implements OnInit {
  @Input() recipe: RecipeModel;
  @Input() id: number;

  constructor(private recipesService: RecipesService) { }

  ngOnInit() {
  }

  // onSelected() {
  //   this.recipesService.updateSelectedRecipe(this.recipe);
  // }
}
