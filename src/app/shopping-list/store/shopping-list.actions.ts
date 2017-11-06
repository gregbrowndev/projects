import {Action} from '@ngrx/store';
import {IngredientModel} from '../../shared/ingredient.model';

export const ADD_INGREDIENT = 'ADD_INGREDIENT';
export const ADD_INGREDIENTS = 'ADD_INGREDIENTS';

export class AddIngredient implements Action {
  readonly type = ADD_INGREDIENT;

  constructor(public payload: IngredientModel) {
    console.log('Action constructed', payload);
  }
}

export class AddIngredients implements Action {
  readonly type = ADD_INGREDIENTS;

  constructor(public payload: IngredientModel[]) {
    console.log('Action constructed', payload);
  }
}

// Bundle all exported Actions into a single exported type
export type ShoppingListActions = AddIngredient | AddIngredients;
