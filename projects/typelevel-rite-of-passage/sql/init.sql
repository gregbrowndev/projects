CREATE DATABASE jobsboard;
\c jobsboard;

CREATE TABLE job (
  id uuid PRIMARY KEY,
  date timestamp NOT NULL,
  ownerEmail text NOT NULL,
  active boolean,
  company text NOT NULL,
  title text NOT NULL,
  description text NOT NULL,
  seniority text,
  remote boolean NOT NULL,
  office text NOT NULL,
  country text,
  salaryLo integer,
  salaryHi integer NOT NULL,
  currency text NOT NULL,
  externalUrl text NOT NULL,
  image text,
  tags text [],
  other text
);
