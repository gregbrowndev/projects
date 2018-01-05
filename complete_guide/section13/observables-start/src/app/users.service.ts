import { Injectable } from '@angular/core';
import {Subject} from 'rxjs/Subject';

@Injectable()
export class UsersService {
  userActivated = new Subject();

  constructor() { }

}
