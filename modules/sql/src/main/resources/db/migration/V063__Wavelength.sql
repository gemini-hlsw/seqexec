--
-- Adds comments to various tables, columns, types.
--

-- Switch GOMS to nm, with precision to distinguish pm 
-- 10,3 can hold [0, 2147483647] pm as nm

ALTER TABLE smart_gmos_north
  ALTER COLUMN min_wavelength SET DATA TYPE numeric(10,3),
  ALTER COLUMN max_wavelength SET DATA TYPE numeric(10,3);

COMMENT ON COLUMN smart_gmos_north.min_wavelength IS 'nm';
COMMENT ON COLUMN smart_gmos_north.max_wavelength IS 'nm';

ALTER TABLE smart_gmos_south
  ALTER COLUMN min_wavelength SET DATA TYPE numeric(10,3),
  ALTER COLUMN max_wavelength SET DATA TYPE numeric(10,3);

COMMENT ON COLUMN smart_gmos_south.min_wavelength IS 'nm';
COMMENT ON COLUMN smart_gmos_south.max_wavelength IS 'nm';

ALTER TABLE step_gmos_north
  ALTER COLUMN wavelength SET DATA TYPE numeric(10,3);

COMMENT ON COLUMN step_gmos_north.wavelength IS 'nm';

ALTER TABLE step_gmos_south
  ALTER COLUMN wavelength SET DATA TYPE numeric(10,3);

COMMENT ON COLUMN step_gmos_south.wavelength IS 'nm';

-- Switch GNIRS to μm, with precision to distinguish pm
-- 10,3 can hold [0, 2147483647] pm as μm

ALTER TABLE smart_gnirs
  ALTER COLUMN min_wavelength SET DATA TYPE numeric(10,6),
  ALTER COLUMN max_wavelength SET DATA TYPE numeric(10,6);

COMMENT ON COLUMN smart_gnirs.min_wavelength      IS 'μm';
COMMENT ON COLUMN smart_gnirs.max_wavelength      IS 'μm';

ALTER TABLE step_gnirs
  ALTER COLUMN wavelength SET DATA TYPE numeric(10,6);

COMMENT ON COLUMN step_gnirs.wavelength      IS 'μm';


