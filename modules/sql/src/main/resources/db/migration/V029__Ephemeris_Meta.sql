
--
-- Ephemeris metadata used to report upon last update times and, for comets
-- and asteroids, avoid unnecessary updates.
--

CREATE TABLE ephemeris_meta (
    key_type          identifier  REFERENCES e_ephemeris_type,
    key               text        NOT NULL,
    site              identifier  REFERENCES e_site,
    last_update       timestamptz NOT NULL,
    last_update_check timestamptz NOT NULL,
    horizons_soln_ref text,
    PRIMARY KEY (key_type, key, site)
);

ALTER TABLE ephemeris_meta OWNER TO postgres;

COMMENT ON COLUMN ephemeris_meta.horizons_soln_ref IS 'horizons-defined version for comet and asteroid ephemeris calculations';

