import {Component, OnInit} from '@angular/core';
import {Response} from '@angular/http';
import 'rxjs/Rx';
import {RecipesService} from '../recipes/recipes.service';
import {ShoppingListService} from '../shopping-list/shopping-list.service';
import {AuthService} from "../auth/auth.service";

@Component({
  selector: 'app-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.css']
})
export class HeaderComponent implements OnInit {

  constructor(private recipesService: RecipesService,
              private authService: AuthService) { }

  ngOnInit() {
  }

  onSave() {
    this.recipesService.save();
  }

  onFetch() {
    this.recipesService.fetch();
  }

  onLogout() {
    this.authService.logout();
  }
}
