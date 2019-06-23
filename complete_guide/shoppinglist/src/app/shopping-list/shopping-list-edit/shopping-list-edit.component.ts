import {Component, ElementRef, EventEmitter, OnInit, Output, ViewChild} from '@angular/core';
import {IngredientModel} from '../../shared/ingredient.model';

@Component({
  selector: 'app-shopping-list-edit',
  templateUrl: './shopping-list-edit.component.html',
  styleUrls: ['./shopping-list-edit.component.css']
})
export class ShoppingListEditComponent implements OnInit {
  @ViewChild('nameInput') nameInput: ElementRef;
  @ViewChild('amountInput') amountInput: ElementRef;
  @Output() ingredientAdded = new EventEmitter<IngredientModel>();

  constructor() { }

  ngOnInit() {
  }

  onAdd() {
    const newIngredient = new IngredientModel(
      this.nameInput.nativeElement.value,
      this.amountInput.nativeElement.value
    );
    this.ingredientAdded.emit(newIngredient);
  }
}
