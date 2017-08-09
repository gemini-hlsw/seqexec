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

-- Make the FPU table name match the corresponding gmos versions.
ALTER TABLE e_f2_fpunit RENAME TO e_f2_fpu;

-- Remove the None and Custom "FPUs".
DELETE FROM e_f2_fpu
      WHERE id = 'None' OR id = 'Custom';

COMMENT ON COLUMN e_f2_fpu.slit_width IS 'pixels';

-- The builtin FPU is now optional and the choice to use a custom mask may
-- be registered.  If a builtin is set, custom_mask must be false.
ALTER TABLE step_f2
      ALTER COLUMN fpu DROP NOT NULL,
        ADD COLUMN custom_mask boolean NOT NULL,
	ADD CONSTRAINT f2_fpu_check CHECK ((fpu IS NULL) OR (custom_mask = 'f'));

-- The filter wavelength should be precise μm.
ALTER TABLE e_f2_filter
      ALTER COLUMN wavelength TYPE NUMERIC(4,3);

COMMENT ON COLUMN e_f2_filter.wavelength IS 'μm';

