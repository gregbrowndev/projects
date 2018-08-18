# Scrapy Bikeshare

## Overview


## Database

The database is accessed via SqlAlchemy and
the schema is managed using Alembic.

**Initialise Alembic**

```shell
alembic init db
```

**Create Migration**

```shell
alembic revision --autogenerate -m "create system table"
```


**Run Migration**

Migrate to latest (head):

```shell
alembic upgrade head
```