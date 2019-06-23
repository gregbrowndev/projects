import {Component, EventEmitter, Input, OnInit, Output} from '@angular/core';
import {RecipeModel} from '../../recipe.model';

@Component({
  selector: 'app-recipe-item',
  templateUrl: './recipe-item.component.html',
  styleUrls: ['./recipe-item.component.css']
})
export class RecipeListItemComponent implements OnInit {
  @Input() recipe: RecipeModel;
  @Output() recipeSelected = new EventEmitter<RecipeModel>();

  constructor() { }

  ngOnInit() {
  }

  onSelected() {
    this.recipeSelected.emit(this.recipe);
  }
}
