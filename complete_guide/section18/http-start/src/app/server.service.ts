import { Injectable } from '@angular/core';
import {Http, Headers, Response} from '@angular/http';
import 'rxjs/Rx';
import {Observable} from "rxjs/Observable";

@Injectable()
export class ServerService {

  constructor(private http: Http) { }

  storeServers(servers: any[]) {
    const headers = new Headers({'Content-Type': 'application/json'});
    return this.http.post('www.url-to-backend.com/mydata', servers, {headers: headers});
  }

  getServers() {
    return this.http.get('www.url-to-backend.com/mydata')
      .map(
        (response: Response) => {
          const data = response.json();
          return data;
        }
      )
      .catch(
        (error: Response) => {
          console.log(error);
          return Observable.throw(error);
        }
      );
  }

  getAppName() {
    return this.http.get('www.url-to-backend.com/appName')
      .map(
        (response: Response) => {
          return response.json();
        }
      );
  }
}
