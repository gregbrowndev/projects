# TransitGTFS

This app is a simple wrapper around the now unmaintained [MultiGTFS](https://multigtfs.readthedocs.io/en/latest/) app, which provides models and utilities for GTFS-formatted data, such as importing, exporting and refreshing shape geometries (see [docs](https://multigtfs.readthedocs.io/en/latest/usage.html#management-commands) and the GTFS commands section below).

## Install Docker

Firstly, install Docker and Compose. 

For Windows, see the [instructions](https://docs.docker.com/docker-for-windows/install/), and click 'Get Docker for Windows (Stable)', which will install everything in a single download. 

On Linux, follow the [instructions](https://docs.docker.com/engine/installation/), picking the platform, e.g. [Ubuntu](https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/), [Fedora](https://docs.docker.com/engine/installation/linux/docker-ce/fedora/), [CentOS](https://docs.docker.com/engine/installation/linux/docker-ce/centos/) etc. You will need to install Compose separately.

## Deploy 

The app can be deployed using Docker Compose:

```console
docker-compose up -d --build
```

Bring down the app:

```console
docker-compose down
```

## Django Tasks

### Admin

When you first run the app and a fresh database is created, you will need to create a superuser to be able to login into Django's admin site:

```console
docker container exec -it transitgtfs_web_1 ./manage.py createsuperuser
```

Note, if the root directory is named something other than 'transitgtfs' you can do:

```console
docker-compose ps
```

to list the processes currently deployed by Compose. There should be one container with 'web' in it. Run the command above with this container's name.

> Note, you can replace `./manage.py createsuperuser` with other commands, such as `bash`, to get a bash shell into the container, or `./manage.py makemigrations` to create a migration file. The `migrate` command should happen automatically as part of the build/deployment. 


### GTFS Commands

Import a GTFS zip file or directory

```console
docker container exec -it transitgtfs_web_1 importgtfs [GTFS zip/directory] --name [Feed Name]
```

Example, importing 'Washington.zip'
```console
docker container exec -it transitgtfs_web_1 importgtfs Washington.zip --name Washington
```

## Webpages

List of websites:

* Django Admin: `localhost:8000/admin`
* PgAdmin: `localhost:5050`
* Adminer: `localhost:8080`

PgAdmin and Adminer are database management tools, which allow you to run SQL queries and other tasks on the database.

To connect with PgAdmin:

* login using default email `pgadmin4@pgadmin.org`  and password `admin`
* connect to the DB using the settings below (note use host `db` and port `5432`)  

## Database Settings

* host: `db` 
* port: `5432`
* postgres user: `postgres`
* postgres password: `mypwd`
* postgres db: `postgres`  

Note, connections to the database can also be made from outside the Dockerised services (i.e. Django/PgAdmin/Adminer). Example - to connect with Arc running locally use:

* host: `localhost`
* port: `54321`