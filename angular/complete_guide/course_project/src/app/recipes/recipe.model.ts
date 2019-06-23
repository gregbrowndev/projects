import {IngredientModel} from '../shared/ingredient.model';

export class RecipeModel {
  public name: string;
  public description: string;
  public imagePath: string;
  public ingredients: IngredientModel[];

  constructor(name: string, desc: string, imagePath: string, ingredients: IngredientModel[]) {
    this.name = name;
    this.description = desc;
    this.imagePath = imagePath;
    this.ingredients = ingredients;
  }
}
