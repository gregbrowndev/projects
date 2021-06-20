#!/usr/bin/env bash

sudo apt-get update

# Install Postgresql
sudo apt-get install postgresql postgresql-contrib
sudo apt-get install postgresql-server-dev-9.5 # NOTE - this needs to be the same version as the version of postgresql
sudo apt-get install python-psycopg2

# Install Geospatial libraries, see https://docs.djangoproject.com/en/1.11/ref/contrib/gis/install/geolibs/#installing-geospatial-libraries
sudo apt-get install binutils libproj-dev gdal-bin postgis


# Create database

#psql -U postgres postgres -f db_create.sql