import {Injectable} from '@angular/core';
import {IngredientModel} from '../shared/ingredient.model';
import {Subject} from 'rxjs/Subject';
import {Http, Response} from '@angular/http';
import 'rxjs/Rx';

@Injectable()
export class ShoppingListService {
  ingredientsChanged = new Subject<IngredientModel[]>();
  startedEditting = new Subject<number>();

  private ingredients: IngredientModel[] = [
    new IngredientModel('Apples', 5),
    new IngredientModel('Tomatoes', 10),
  ];

  constructor(private http: Http) { }


  getIngredient(index: number) {
    return this.ingredients[index];
  }

  pushIngredients() {
    this.ingredientsChanged.next(this.ingredients.slice());
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

  deleteIngredient(index: number) {
    this.ingredients.splice(index, 1);
    this.pushIngredients();
  }
}
