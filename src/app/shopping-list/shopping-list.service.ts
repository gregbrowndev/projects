import {Injectable} from '@angular/core';
import {IngredientModel} from '../shared/ingredient.model';
import {Subject} from 'rxjs/Subject';

@Injectable()
export class ShoppingListService {
  ingredientsChanged = new Subject<IngredientModel[]>();
  startedEditting = new Subject<number>();

  private ingredients: IngredientModel[] = [
    new IngredientModel('Apples', 5),
    new IngredientModel('Tomatoes', 10),
  ];

  constructor() { }

  getIngredients() {
    return this.ingredients.slice();
  }

  getIngredient(index: number) {
    return this.ingredients[index];
  }

  pushIngredients() {
    this.ingredientsChanged.next(this.ingredients.slice());
  }

  addIngredient(ingredient: IngredientModel) {
    console.log(ingredient);
    this.ingredients.push(ingredient);
    this.pushIngredients();
  }

  addIngredients(ingredients: IngredientModel[]) {
    // Viable option but may lead to unnecessary event emission
    // for (const ingredient of ingredients) {
    //   this.addIngredient(ingredient);
    // }
    this.ingredients.push(...ingredients);
    this.pushIngredients();
  }

  updateIngredient(index: number, ingredient: IngredientModel) {
    this.ingredients[index] = ingredient;
    this.pushIngredients();
  }
}
