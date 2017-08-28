
--
-- Horizons non-sidereal target types.
--

CREATE TABLE e_horizons_type (
    id   identifier            PRIMARY KEY,
    name character varying(12) NOT NULL
);

ALTER TABLE e_horizons_type OWNER TO postgres;


COPY e_horizons_type (id, name) FROM stdin;
Comet	Comet
AsteroidNew	Asteroid New
AsteroidOld	Asteroid Old
MajorBody	Major Body
\.

