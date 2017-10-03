import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';
import { HeaderComponent } from './header/header.component';
import { ShoppingListComponent } from './shopping-list/shopping-list.component';
import { ShoppingListItemComponent } from './shopping-list/shopping-list-item/shopping-list-item.component';
import { ShoppingListEditComponent } from './shopping-list/shopping-list-edit/shopping-list-edit.component';
import { RecipesComponent } from './recipes/recipes.component';
import { RecipeListComponent } from './recipes/recipe-list/recipe-list.component';
import { RecipeDetailComponent } from './recipes/recipe-detail/recipe-detail.component';
import { RecipeListItemComponent } from './recipes/recipe-list/recipe-item/recipe-item.component';
import { DropdownDirective } from './shared/dropdown.directive';
import {ShoppingListService} from './shopping-list/shopping-list.service';
import {AppRoutingModule} from './app-routing.module';
import { ErrorPageComponent } from './shared/error-page/error-page.component';
import { RecipeStartComponent } from './recipes/recipe-start/recipe-start.component';
import { RecipeEditComponent } from './recipes/recipe-edit/recipe-edit.component';
import {FormsModule} from '@angular/forms';

@NgModule({
  declarations: [
    AppComponent,
    ShoppingListComponent,
    ShoppingListItemComponent,
    ShoppingListEditComponent,
    HeaderComponent,
    RecipesComponent,
    RecipeListComponent,
    RecipeDetailComponent,
    RecipeListItemComponent,
    DropdownDirective,
    ErrorPageComponent,
    RecipeStartComponent,
    RecipeEditComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    FormsModule
  ],
  providers: [ShoppingListService],
  bootstrap: [AppComponent]
})
export class AppModule { }
