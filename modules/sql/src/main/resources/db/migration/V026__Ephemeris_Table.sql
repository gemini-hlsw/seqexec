
--
-- Ephemeris
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
    CONSTRAINT dec_str_check CHECK (dec_str ~ '^[+-]\d\d:\d\d:\d\d\.\d\d\d\d\d\d$')
);

ALTER TABLE ephemeris OWNER TO postgres;

COMMENT ON COLUMN ephemeris.ra      IS 'µs';
COMMENT ON COLUMN ephemeris.dec     IS 'µas';
COMMENT ON COLUMN ephemeris.ra_str  IS 'HH:MM:SS.µµµµµµ';
COMMENT ON COLUMN ephemeris.dec_str IS '[+-]DD:MM:SS.µµµµµµ';

CREATE INDEX ephemeris_index ON ephemeris (key_type, key, timestamp);

CREATE SEQUENCE user_ephemeris_id
    MINVALUE 0
    MAXVALUE 2147483647
    START WITH 0
    NO CYCLE;

ALTER SEQUENCE user_ephemeris_id OWNER TO postgres;
