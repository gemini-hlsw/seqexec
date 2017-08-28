
--
-- Horizons non-sidereal target types.
--

CREATE TABLE e_horizons_type (
    id         identifier            PRIMARY KEY,
    short_name character varying(12) NOT NULL,
    long_name  character varying(21) NOT NULL
);

ALTER TABLE e_horizons_type OWNER TO postgres;


COPY e_horizons_type (id, short_name, long_name) FROM stdin;
Comet	Comet	Comet
AsteroidNew	Asteroid New	Asteroid (New Format)
AsteroidOld	Asteroid Old	Asteroid (Old Format)
MajorBody	Major Body	Major Body
\.

