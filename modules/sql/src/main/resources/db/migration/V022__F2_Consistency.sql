--
-- Updates to F2 to make it consistent with GMOS
--

-- Remove the "NoDisperser" option.
DELETE FROM e_f2_disperser
      WHERE id = 'NoDisperser';

-- The disperser wavelength should be precise μm and not nullable.

ALTER TABLE e_f2_disperser
      ALTER COLUMN wavelength TYPE NUMERIC(4,3),
      ALTER COLUMN wavelength SET NOT NULL;

COMMENT ON COLUMN e_f2_disperser.wavelength IS 'μm';

-- F2 dynamic config may or may not have a disperser.

ALTER TABLE step_f2
      ALTER COLUMN disperser DROP NOT NULL;




