-- Track type, which is used as a tag. This is not strictly necessary but will make it easier to
-- write queries later on.

CREATE TYPE e_track_type AS ENUM ('Sidereal', 'Nonsidereal');

-- Target table, which is mostly the Track encoding as a coproduct. So either the proper motion
-- columns will be all NULL or the ephemeris key columns will be all NULL. A check constraint
-- guarantees this. A tag tells us which to expect.

CREATE TABLE target (
  id SERIAL PRIMARY KEY,
  name VARCHAR(64) NOT NULL,

  -- tag
  track_type e_track_type  NOT NULL,

  -- proper motion base coordinates, human-readable
  ra_str     VARCHAR(15)   NULL,
  dec_str    VARCHAR(16)   NULL,

  CONSTRAINT ra_str_check  CHECK (ra_str  ~     '^\d\d:\d\d:\d\d\.\d\d\d\d\d\d$'),
  CONSTRAINT dec_str_check CHECK (dec_str ~ '^[+-]\d\d:\d\d:\d\d\.\d\d\d\d\d\d$'),

  -- proper motion columns
  ra         BIGINT        NULL,
  dec        BIGINT        NULL,
  epoch      VARCHAR(9)    NULL,
  pv_ra      BIGINT        NULL,
  pv_dec     BIGINT        NULL,
  rv         INT           NULL,
  px         BIGINT        NULL,

  -- ephemeris columns
  e_key_type identifier    NULL REFERENCES e_ephemeris_type,
  e_key      text          NULL,

  -- ensure that the coproduct encoding is consistent
  CONSTRAINT target_coproduct CHECK ((
    track_type = 'Sidereal'
      AND e_key_type IS NULL
      AND e_key      IS NULL
      AND ra_str     IS NOT NULL
      AND dec_str    IS NOT NULL
      AND ra         IS NOT NULL
      AND dec        IS NOT NULL
      AND epoch      IS NOT NULL
      -- remaining columns can be null
  ) OR (
    track_type = 'Nonsidereal'
      AND e_key_type IS NOT NULL
      AND e_key      IS NOT NULL
      AND ra_str     IS NULL
      AND dec_str    IS NULL
      AND ra         IS NULL
      AND dec        IS NULL
      AND epoch      IS NULL
      AND pv_ra      IS NULL
      AND pv_dec     IS NULL
      AND rv         IS NULL
      AND px         IS NULL
  ))

);

COMMENT ON COLUMN target.name       IS 'human-readable target name';
COMMENT ON COLUMN target.track_type IS 'track type, which determines nullability of following columns';
COMMENT ON COLUMN target.ra_str     IS '(sidereal) human-readable right ascension as hh:mm:ss.µµµµµµ';
COMMENT ON COLUMN target.dec_str    IS '(sidereal) human-readable declination as +dd:mm:ss.µµµµµµ';
COMMENT ON COLUMN target.ra         IS '(sidereal) right ascension in microseconds';
COMMENT ON COLUMN target.dec        IS '(sidereal) declination in microarcseconds';
COMMENT ON COLUMN target.epoch      IS '(sidereal) epoch as J2000.123';
COMMENT ON COLUMN target.pv_ra      IS '(sidereal) proper velocity in right ascension, in microarcseconds per year';
COMMENT ON COLUMN target.pv_dec     IS '(sidereal) proper velocity in declination, in microarcseconds per year';
COMMENT ON COLUMN target.rv         IS '(sidereal) radial velocity in km/sec, positive if receding';
COMMENT ON COLUMN target.px         IS '(sidereal) parallax in microarcseconds';
COMMENT ON COLUMN target.e_key_type IS '(nonsidereal) ephemeris key type';
COMMENT ON COLUMN target.e_key_type IS '(nonsidereal) ephemeris key';
