--
-- The disperser order can be calculated from the central wavelength
--

ALTER TABLE step_gnirs
  DROP  COLUMN  disperser_order,
  ADD   COLUMN	wavelength	integer	NOT NULL;

COMMENT ON COLUMN step_gnirs.wavelength IS 'µm';
