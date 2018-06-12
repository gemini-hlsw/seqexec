
--
-- Updates target table proper velocity and parallax from (unsigned) bigint
-- microarcseconds to (signed) numeric milliarcseconds.
--

ALTER TABLE target
  ALTER COLUMN pv_ra  SET DATA TYPE numeric(9,3),
  ALTER COLUMN pv_dec SET DATA TYPE numeric(9,3),
  ALTER COLUMN px     SET DATA TYPE numeric(9,3);

COMMENT ON COLUMN target.pv_ra  IS '(sidereal) proper velocity in right ascension, in milliarcseconds per year';
COMMENT ON COLUMN target.pv_dec IS '(sidereal) proper velocity in declination, in milliarcseconds per year';
COMMENT ON COLUMN target.px     IS '(sidereal) parallax in milliarcseconds';

