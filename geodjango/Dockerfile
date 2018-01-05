FROM python:3

# Install PostgreSQLm PostGIS and Geospatial libraries, see https://docs.djangoproject.com/en/1.11/ref/contrib/gis/install/geolibs/#installing-geospatial-libraries
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
	    binutils \
        gdal-bin \
        libproj-dev \
        postgis \
		postgresql-client \
	&& rm -rf /var/lib/apt/lists/*

# Install Postgresql
#RUN apt-get update && apt-get install postgresql postgresql-contrib
#sudo apt-get install postgresql-server-dev-9.5 # NOTE - this needs to be the same version as the version of postgresql
#sudo apt-get install python-psycopg2

ENV PYTHONUNBUFFERED 1
RUN mkdir /code
WORKDIR /code
COPY requirements.txt ./
RUN pip install -r requirements.txt
COPY . .

# This copies the sql script to the directory, which gets executed when
# a container runs from the image
RUN mkdir /docker-entrypoint-initdb.d
COPY init-db.sh /docker-entrypoint-initdb.d

EXPOSE 8000
CMD ["python", "manage.py", "runserver", "0.0.0.0:8000"]