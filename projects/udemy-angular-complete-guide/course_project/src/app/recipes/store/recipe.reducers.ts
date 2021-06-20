import {RecipeModel} from '../recipe.model';
import {IngredientModel} from '../../shared/ingredient.model';
import * as RecipesActions from './recipe.actions';
import {AppState} from '../../store/app.reducers';

export interface FeatureState extends AppState {
  recipes: State;
}

export interface State {
  recipes: RecipeModel[];
}

const initialState: State = {
  recipes: [
    new RecipeModel(
      'Pizza',
      'A bumadding pizza.',
      'https://www.bbcgoodfood.com/sites/default/files/recipe-collections/collection-image/' +
      '2013/05/frying-pan-pizza-easy-recipe-collection.jpg',
      [
        new IngredientModel('Pizza Base', 1),
        new IngredientModel('Tomato', 3),
        new IngredientModel('Cheese', 9000)
      ]
    ),
    new RecipeModel('Juicy Lucy',
      'King of burgers.',
      'http://www.alt-gifts.com/wp-content/uploads/2016/09/fast-food.jpg',
      [
        new IngredientModel('Beef Burger', 1),
        new IngredientModel('Buns', 2),
        new IngredientModel('Fries', 20),
        new IngredientModel('Salad', 1)
      ])
  ]
};

export function recipeReducer(state = initialState,
                              action: RecipesActions.RecipesActions) {
  switch (action.type) {
    case RecipesActions.SET_RECIPES:
      return {
        ...state,
        recipes: [...action.payload]
      };
    case RecipesActions.ADD_RECIPE:
      return {
        ...state,
        recipes: [...state.recipes, action.payload]
      };
    case RecipesActions.UPDATE_RECIPE:
      const index = action.payload.index;
      const recipe = state.recipes[index];
      const updatedRecipe = {
        ...recipe,
        ...action.payload.data
      };
      const recipes = [...state.recipes];
      recipes[index] = updatedRecipe;
      return {
        ...state,
        recipes: recipes
      };
    case RecipesActions.DELETE_RECIPE:
      const oldRecipes = [...state.recipes];
      oldRecipes.splice(action.payload, 1);
      return {
        ...state,
        recipes: oldRecipes
      };
    default:
      return state;
  }
}
