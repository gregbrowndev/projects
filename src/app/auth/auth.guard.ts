import {Injectable} from '@angular/core';
import {CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot} from '@angular/router';
import {Store} from '@ngrx/store';

import {AppState} from '../store/app.reducers';
import * as fromAuth from '../auth/store/auth.reducers';

// @Injectable()
// export class AuthGuard implements CanActivate, CanLoad {
//
//   constructor(private authService: AuthService,
//               private router: Router) {}
//
//   canActivate(
//     next: ActivatedRouteSnapshot,
//     state: RouterStateSnapshot): Observable<boolean> | Promise<boolean> | boolean {
//     if (this.authService.isAuthenticated()) {
//       return true;
//     } else {
//       this.router.navigate(['/signin'], {
//         queryParams: {
//           redirect: state.url
//         }
//       });
//       return false;
//     }
//   }
//
//   canLoad(route: Route): Observable<boolean> | Promise<boolean> | boolean {
//     if (this.authService.isAuthenticated()) {
//       return true;
//     } else {
//       // Note - cannot get access to future url like in canActivate as RouterStateSnapshot is not passed in to canLoad.
//       // see https://github.com/angular/angular/issues/10584
//       // possible solution is to store attempted URL in app-wide service which can be retrieved here.
//       this.router.navigate(['/signin']);
//       return false;
//     }
//   }
// }

@Injectable()
export class AuthGuard implements CanActivate {

  constructor(private store: Store<AppState>) {
  }

  canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot) {
    console.log('guard');
    return this.store.select('auth')
      .take(1)
      .map((authState: fromAuth.State) => {
        return authState.authenticated;
      });
  }
}
