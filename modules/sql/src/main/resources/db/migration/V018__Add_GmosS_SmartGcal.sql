--
-- Smart Gcal Tables for GMOS-S
--

CREATE TABLE smart_gmos_south (
  lamp           gcal_lamp_type     NOT NULL,
  baseline       gcal_baseline_type NOT NULL,
  disperser      identifier                  REFERENCES e_gmos_south_disperser ON DELETE CASCADE,
  filter         identifier                  REFERENCES e_gmos_south_filter    ON DELETE CASCADE,
  fpu            identifier                  REFERENCES e_gmos_south_fpu       ON DELETE CASCADE,
  x_binning      identifier         NOT NULL REFERENCES e_gmos_binning         ON DELETE CASCADE,
  y_binning      identifier         NOT NULL REFERENCES e_gmos_binning         ON DELETE CASCADE,
  min_wavelength int                NOT NULL CHECK (min_wavelength >= 0),
  max_wavelength int                NOT NULL CHECK (max_wavelength >= 0),
  amp_gain       identifier         NOT NULL REFERENCES e_gmos_amp_gain        ON DELETE CASCADE,
  gcal_id        integer            NOT NULL REFERENCES gcal                   ON DELETE CASCADE,
  CHECK (min_wavelength <= max_wavelength)
);

ALTER TABLE smart_gmos_south OWNER TO postgres;