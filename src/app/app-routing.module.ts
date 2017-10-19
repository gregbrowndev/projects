import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {ShoppingListComponent} from './shopping-list/shopping-list.component';
import {RecipesComponent} from './recipes/recipes.component';
import {RecipeDetailComponent} from './recipes/recipe-detail/recipe-detail.component';
import {ErrorPageComponent} from './shared/error-page/error-page.component';
import {RecipeStartComponent} from './recipes/recipe-start/recipe-start.component';
import {RecipeEditComponent} from './recipes/recipe-edit/recipe-edit.component';
import {SignupComponent} from './auth/signup/signup.component';
import {SigninComponent} from './auth/signin/signin.component';

const appRouters: Routes = [
  {path: '', redirectTo: '/recipes', pathMatch: 'full'},
  {
    path: 'recipes', component: RecipesComponent, children: [
    // {path: '', pathMatch: 'full', component: ErrorPageComponent, data: {message: 'Please select a Recipe!'}},
    {path: '', pathMatch: 'full', component: RecipeStartComponent},
    {path: 'new', component: RecipeEditComponent},
    {path: ':id', component: RecipeDetailComponent},
    {path: ':id/edit', component: RecipeEditComponent}
  ]
  },
  {path: 'shopping-list', component: ShoppingListComponent},
  {path: 'signup', component: SignupComponent},
  {path: 'signin', component: SigninComponent},
];

@NgModule({
  imports: [
    RouterModule.forRoot(appRouters)
  ],
  exports: [
    RouterModule
  ]
})
export class AppRoutingModule {
}
