#!/bin/bash

set -e

# from https://store.docker.com/images/postgres:
# if you would like to do additional initialization
# in an image derived from this one, add one or more
# *.sql, *.sql.gz, or *.sh scripts under
# /docker-entrypoint-initdb.d (creating the directory if necessary).

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" <<-EOSQL
    CREATE USER geodjango PASSWORD 'geodjango';

    CREATE DATABASE geodjango OWNER geodjango;

    SET CONNECTION TO geodjango;
    CREATE EXTENSION IF NOT EXISTS postgis;
EOSQL