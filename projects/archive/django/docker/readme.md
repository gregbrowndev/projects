This project follows this [tutorial](
https://howchoo.com/g/y2y1mtkznda/getting-started-with-docker-compose-and-django), creating a basic Dockerized Django + PostgreSQL stack.

An empty Django project is created with a Dockerfile to build these source files.

A Compose file uses this image to run the Django container as a web service, including a postgres service which is deployed from Docker Hub.

Finally, a image called pg_data is created with its own Dockerfile (under docker/dockerfiles/pg_data) to act as a persistent data storage container. 