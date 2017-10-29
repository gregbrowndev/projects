import {Component, OnInit} from '@angular/core';
import 'rxjs/Rx';
import {AuthService} from '../../auth/auth.service';
import {RecipesService} from '../../recipes/recipes.service';


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
