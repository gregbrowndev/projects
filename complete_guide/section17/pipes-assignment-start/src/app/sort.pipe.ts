import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'sort'
})
export class SortPipe implements PipeTransform {

  transform(value: any, sortby: string): any {
    return value.sort((a, b) => {
      if (a[sortby] > b[sortby]) {
        return 1;
      } else {
        return -1;
      }
    });
  }
}
