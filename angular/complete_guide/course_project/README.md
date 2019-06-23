# Angular 4 Reference Project

This is my reference Angular project. The app is a shopping list and recipe book, following the Udemy course: 
[The Complete Guide to Angular 2](https://www.udemy.com/the-complete-guide-to-angular-2).

The project has also been extended with Docker for deploying the app as a service and doing automated builds. The image is
hosted on [Docker Hub](https://cloud.docker.com/swarm/gregbrown/repository/docker/gregbrown/angular-reference).

## Docker 

The project has been outfitted with Docker.

Deploy:

```console
docker-compose up -d
```

Build and run as single container:
```console
docker container build -t gregbrown/angular-reference .

docker container run -d gregbrown/angular-reference
```

Deploy in production:

```console
docker-compose -f docker-compose.yml -f docker-compose.prod.yml up
```

Deploy as Stack:

```console
docker-compose -f docker-compose.yml -f docker-compose.prod.yml config > docker-stack.yml

docker stack deploy -c docker-stack.yml angular-reference
```

## Development server

Run `ng serve` for a dev server. Navigate to `http://localhost:4200/`. The app will automatically reload if you change any of the source files.

## Code scaffolding

Run `ng generate component component-name` to generate a new component. You can also use `ng generate directive|pipe|service|class|guard|interface|enum|module`.

## Build

Run `ng build` to build the project. The build artifacts will be stored in the `dist/` directory. Use the `-prod` flag for a production build.

## Running unit tests

Run `ng test` to execute the unit tests via [Karma](https://karma-runner.github.io).

## Running end-to-end tests

Run `ng e2e` to execute the end-to-end tests via [Protractor](http://www.protractortest.org/).
Before running the tests make sure you are serving the app via `ng serve`.

## Further help

To get more help on the Angular CLI use `ng help` or go check out the [Angular CLI README](https://github.com/angular/angular-cli/blob/master/README.md).


