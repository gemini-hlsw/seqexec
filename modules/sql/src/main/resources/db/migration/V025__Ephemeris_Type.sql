
---------------------------
-- Ephemeris Key Types
---------------------------

DROP TABLE e_horizons_type;

CREATE TABLE e_ephemeris_type (
    id         identifier PRIMARY KEY,
    short_name text       NOT NULL,
    long_name  text       NOT NULL
);

ALTER TABLE e_ephemeris_type OWNER TO postgres;


COPY e_ephemeris_type (id, short_name, long_name) FROM stdin;
Comet	Comet	Horizons Comet
AsteroidNew	Asteroid New	Horizons Asteroid (New Format)
AsteroidOld	Asteroid Old	Horizons Asteroid (Old Format)
MajorBody	Major Body	Horizons Major Body
UserSupplied	User Supplied	User Supplied
\.
