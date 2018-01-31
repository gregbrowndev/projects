import {Injectable} from '@angular/core';
import {Subject} from 'rxjs/Subject';

@Injectable()
export class UIService {
  loadingStateChanged = new Subject<boolean>();
}
