import {NgModule} from '@angular/core';
import {DropdownDirective} from './dropdown.directive';
import {CommonModule} from '@angular/common';
import {ErrorPageComponent} from './error-page/error-page.component';

@NgModule({
  declarations:  [
    DropdownDirective,
    ErrorPageComponent
  ],
  imports: [
    CommonModule
  ],
  exports: [
    CommonModule,
    DropdownDirective,
    ErrorPageComponent
  ]
})
export class SharedModule {}
