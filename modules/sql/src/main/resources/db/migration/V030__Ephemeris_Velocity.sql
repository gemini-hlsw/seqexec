
--
-- Ephemeris
--
-- RA and dec are represented in both numeric (µs, µas respectively) and
-- formatted string representation.  Internally we use the numeric value,
-- but the string representation is provided as well for human consumption.
--

ALTER TABLE ephemeris
   ADD COLUMN delta_ra  NUMERIC(10,6) NOT NULL,
   ADD COLUMN delta_dec NUMERIC(10,6) NOT NULL;


COMMENT ON COLUMN ephemeris.delta_ra  IS 'arcsec / hour * cos(dec)';
COMMENT ON COLUMN ephemeris.delta_dec IS 'arcsec / hour';
