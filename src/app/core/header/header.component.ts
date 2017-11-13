import {Component, OnInit} from '@angular/core';
import 'rxjs/Rx';
import {Store} from '@ngrx/store';
import {Observable} from 'rxjs/Observable';
import {AuthService} from '../../auth/auth.service';
import {RecipesService} from '../../recipes/recipes.service';
import {AppState} from '../../store/app.reducers';
import * as fromAuth from '../../auth/store/auth.reducers';
import * as AuthActions from '../../auth/store/auth.actions';


@Component({
  selector: 'app-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.css']
})
export class HeaderComponent implements OnInit {
  authState: Observable<fromAuth.State>;

  constructor(private recipesService: RecipesService,
              public authService: AuthService,
              private store: Store<AppState>) { }

  ngOnInit() {
    this.authState = this.store.select('auth');
  }

  onSave() {
    this.recipesService.save();
  }

  onFetch() {
    this.recipesService.fetch();
  }

  onLogout() {
    // this.authService.logout();
    this.store.dispatch(new AuthActions.TrySignout());
  }
}
