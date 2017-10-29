import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import {ShoppingListEditComponent} from './shopping-list-edit/shopping-list-edit.component';
import {ShoppingListItemComponent} from './shopping-list-item/shopping-list-item.component';
import {ShoppingListComponent} from './shopping-list.component';
import {FormsModule} from '@angular/forms';
import {ShoppingListRoutingModule} from './shopping-list-routing.module';
import {SharedModule} from '../shared/shared.module';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    SharedModule,
    ShoppingListRoutingModule
  ],
  declarations: [
    ShoppingListComponent,
    ShoppingListItemComponent,
    ShoppingListEditComponent,
  ]
})
export class ShoppingListModule { }
