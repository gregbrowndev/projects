import {Directive, HostBinding, HostListener} from '@angular/core';

@Directive({
  selector: '[appDropdown]'
})
export class DropdownDirective {

  @HostBinding('class.open') isOpen = false;

  constructor() { }

  @HostListener('click') click (eventData: Event) {
    this.isOpen = !this.isOpen;
  }

  @HostListener('mouseleave') mouseleave (eventData: Event) {
    this.isOpen = false;
  }

}
