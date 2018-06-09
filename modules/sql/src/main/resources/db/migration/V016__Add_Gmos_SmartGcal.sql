--
-- Smart Gcal Tables for GMOS
--

CREATE TABLE smart_gmos_north (
  lamp           gcal_lamp_type     NOT NULL,
  baseline       gcal_baseline_type NOT NULL,
  disperser      identifier                  REFERENCES e_gmos_north_disperser ON DELETE CASCADE,
  filter         identifier                  REFERENCES e_gmos_north_filter    ON DELETE CASCADE,
  fpu            identifier                  REFERENCES e_gmos_north_fpu       ON DELETE CASCADE,
  x_binning      identifier         NOT NULL REFERENCES e_gmos_binning         ON DELETE CASCADE,
  y_binning      identifier         NOT NULL REFERENCES e_gmos_binning         ON DELETE CASCADE,
  min_wavelength int                NOT NULL CHECK (min_wavelength >= 0),
  max_wavelength int                NOT NULL CHECK (max_wavelength >= 0),
  amp_gain       identifier         NOT NULL REFERENCES e_gmos_amp_gain        ON DELETE CASCADE,
  gcal_id        integer            NOT NULL REFERENCES gcal                   ON DELETE CASCADE,
  CHECK (min_wavelength <= max_wavelength)
);

ALTER TABLE smart_gmos_north OWNER TO postgres;

--
-- Store wavelength in integer angstroms instead of double nm
--

ALTER TABLE step_gmos_north
  ALTER COLUMN wavelength SET DATA TYPE integer USING round(wavelength * 10.0);

ALTER TABLE step_gmos_south
  ALTER COLUMN wavelength SET DATA TYPE integer USING round(wavelength * 10.0);