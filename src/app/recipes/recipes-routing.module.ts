import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {RecipesComponent} from './recipes.component';
import {RecipeStartComponent} from './recipe-start/recipe-start.component';
import {RecipeEditComponent} from './recipe-edit/recipe-edit.component';
import {AuthGuard} from '../auth/auth.guard';
import {RecipeDetailComponent} from './recipe-detail/recipe-detail.component';


const recipeRoutes: Routes = [
  { path: '', component: RecipesComponent, children: [
    // {path: '', pathMatch: 'full', component: ErrorPageComponent, data: {message: 'Please select a Recipe!'}},
    {path: '', pathMatch: 'full', component: RecipeStartComponent},
    {path: 'new', component: RecipeEditComponent, canActivate: [AuthGuard]},
    {path: ':id', component: RecipeDetailComponent},
    {path: ':id/edit', component: RecipeEditComponent, canActivate: [AuthGuard]}
  ]},
];

@NgModule({
  imports: [
    RouterModule.forChild(recipeRoutes)
  ],
  exports: [
    RouterModule
  ]
})
export class RecipesRoutingModule {

}
