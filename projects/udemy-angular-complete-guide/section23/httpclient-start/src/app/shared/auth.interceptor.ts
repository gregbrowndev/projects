import {HttpEvent, HttpHandler, HttpInterceptor, HttpRequest} from '@angular/common/http';
import {Observable} from 'rxjs/Observable';
import {Injectable} from '@angular/core';
import {AuthService} from '../auth/auth.service';

@Injectable()
export class AuthInterceptor implements HttpInterceptor {

  constructor(private authService: AuthService) { }

  intercept(req: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any> > {
    const copiedReq = req.clone({
      params: req.params.append('auth', this.authService.getToken()),
      // headers: req.headers.append('', ''),
    });
    return next.handle(copiedReq);
  }
}
