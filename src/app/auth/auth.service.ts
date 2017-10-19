import { Injectable } from '@angular/core';
import * as firebase from 'firebase';

@Injectable()
export class AuthService {
  token: string;

  constructor() { }

  signupUser(email: string, password: string) {
    firebase.auth().createUserWithEmailAndPassword(email, password)
      .catch(
        error => console.log(error)
      );
  }

  signinUser(email: string, password: string) {
    firebase.auth().signInWithEmailAndPassword(email, password)
      .then(
        response => this.getToken()
      )
      .catch(
        error => console.log(error)
      );
  }

  getToken() {
    firebase.auth().currentUser.getToken()
      .then(
        (token: string) => {
          console.log(token);
          this.token = token;
        }
      );
    return this.token;
  }
}
