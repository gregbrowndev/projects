
export class CounterService {
  counter = 0;

  constructor() { }

  incCounter() {
    ++this.counter;
    console.log('Counter: ' + this.counter);
  }

}
