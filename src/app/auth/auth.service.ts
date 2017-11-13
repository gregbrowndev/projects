import { Injectable } from '@angular/core';
import * as firebase from 'firebase';
import {Store} from '@ngrx/store';
import {Router} from '@angular/router';

import * as fromAuth from './store/auth.reducers';
import {AppState} from '../store/app.reducers';
import * as AuthActions from './store/auth.actions';
import {Observable} from 'rxjs/Observable';


@Injectable()
export class AuthService {

  constructor(private router: Router,
              private store: Store<AppState>) { }

}
