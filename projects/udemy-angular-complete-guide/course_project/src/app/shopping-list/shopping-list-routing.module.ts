import {NgModule} from '@angular/core';
import {RouterModule, Routes} from '@angular/router';
import {ShoppingListComponent} from './shopping-list.component';

const shoppingListRouters: Routes = [
  {path: '', component: ShoppingListComponent},
];

@NgModule({
  imports: [
    RouterModule.forChild(shoppingListRouters)
  ],
  exports: [RouterModule]
})
export class ShoppingListRoutingModule {}
