CREATE USER geodjango PASSWORD 'geodjango';

CREATE DATABASE geodjango OWNER geodjango;
SET CONNECTION TO geodjango;

CREATE EXTENSION IF NOT EXISTS postgis;
