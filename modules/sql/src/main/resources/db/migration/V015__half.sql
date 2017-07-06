CREATE TYPE half AS ENUM (
    'A', 'B'
);

ALTER TABLE semester
  ALTER COLUMN half
    TYPE half
    USING half::half
