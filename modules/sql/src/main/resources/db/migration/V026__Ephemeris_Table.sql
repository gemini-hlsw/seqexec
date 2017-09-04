
--
-- Ephemeris
--
-- RA and dec are represented in both numeric (µs, µas respectively) and
-- formatted string representation.  Internally we use the numeric value,
-- but the string representation is provided as well for human consumption.
--

CREATE TABLE ephemeris (
    key_type   identifier REFERENCES e_ephemeris_type ON DELETE CASCADE,
    key        text        NOT NULL,
    timestamp  timestamptz NOT NULL,
    ra         bigint      NOT NULL,
    dec        bigint      NOT NULL,
    ra_str     varchar(15) NOT NULL,
    dec_str    varchar(16) NOT NULL,
    CONSTRAINT ra_str_check  CHECK (ra_str  ~     '^\d\d:\d\d:\d\d\.\d\d\d\d\d\d$'),
    CONSTRAINT dec_str_check CHECK (dec_str ~ '^[+-]\d\d:\d\d:\d\d\.\d\d\d\d\d\d$'),
    PRIMARY KEY (key_type, key, timestamp)
);

ALTER TABLE ephemeris OWNER TO postgres;

COMMENT ON COLUMN ephemeris.ra      IS 'µs';
COMMENT ON COLUMN ephemeris.dec     IS 'µas';
COMMENT ON COLUMN ephemeris.ra_str  IS 'HH:MM:SS.µµµµµµ';
COMMENT ON COLUMN ephemeris.dec_str IS '[+-]DD:MM:SS.µµµµµµ';


--
-- Sequence for UserSupplied keys.  Here we set the max value to Int.MaxValue
-- so that we can store the resulting value in an Int.
--

CREATE SEQUENCE user_ephemeris_id
    MINVALUE 0
    MAXVALUE 2147483647
    START WITH 0
    NO CYCLE;

ALTER SEQUENCE user_ephemeris_id OWNER TO postgres;
