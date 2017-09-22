import {EventEmitter, Injectable} from '@angular/core';
import {IngredientModel} from '../shared/ingredient.model';

@Injectable()
export class ShoppingListService {
  private ingredients: IngredientModel[] = [
    new IngredientModel('Apples', 5),
    new IngredientModel('Tomatoes', 10),
  ];

  ingredientsChanged = new EventEmitter<IngredientModel[]>();

  constructor() { }

  getIngredients() {
    return this.ingredients.slice();
  }

  addIngredient(ingredient: IngredientModel) {
    console.log(ingredient);
    this.ingredients.push(ingredient);
    this.ingredientsChanged.emit(this.ingredients);
  }

  addIngredients(ingredients: IngredientModel[]) {
    // Viable option but may lead to unnecessary event emission
    // for (const ingredient of ingredients) {
    //   this.addIngredient(ingredient);
    // }
    this.ingredients.push(...ingredients);
    this.ingredientsChanged.emit(this.ingredients);
  }
}
