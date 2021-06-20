// The file contents for the current environment will overwrite these during build.
// The build system defaults to the dev environment which uses `environment.ts`, but if you do
// `ng build --env=prod` then `environment.prod.ts` will be used instead.
// The list of which env maps to which file can be found in `.angular-cli.json`.

export const environment = {
  production: false,
  firebase: {
    apiKey: 'AIzaSyApxZtsQ9yei6EqQLTytlGe44VuwXxtek4',
    authDomain: 'ng-fitness-tracker-9fbb9.firebaseapp.com',
    databaseURL: 'https://ng-fitness-tracker-9fbb9.firebaseio.com',
    projectId: 'ng-fitness-tracker-9fbb9',
    storageBucket: 'ng-fitness-tracker-9fbb9.appspot.com',
    messagingSenderId: '1053083862045'
  }
};
