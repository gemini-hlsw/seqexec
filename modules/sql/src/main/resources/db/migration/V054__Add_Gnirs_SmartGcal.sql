--
-- Name: smart_gnirs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE smart_gnirs (
    lamp               gcal_lamp_type     NOT NULL,
    baseline           gcal_baseline_type NOT NULL,
    acquisition_mirror identifier         NOT NULL  REFERENCES e_gnirs_acquisition_mirror ON DELETE CASCADE,
    pixel_scale        identifier         NOT NULL  REFERENCES e_gnirs_pixel_scale        ON DELETE CASCADE,
    disperser          identifier         NOT NULL  REFERENCES e_gnirs_disperser          ON DELETE CASCADE,
    fpu_slit           identifier                   REFERENCES e_gnirs_fpu_slit           ON DELETE CASCADE,
    fpu_other          identifier                   REFERENCES e_gnirs_fpu_other          ON DELETE CASCADE,
    prism              identifier         NOT NULL  REFERENCES e_gnirs_prism              ON DELETE CASCADE,
    well_depth         identifier         NOT NULL  REFERENCES e_gnirs_well_depth         ON DELETE CASCADE,
    min_wavelength     int                NOT NULL  CHECK (min_wavelength >= 0),
    max_wavelength     int                NOT NULL  CHECK (max_wavelength >= 0),
    gcal_id            integer            NOT NULL  REFERENCES gcal                       ON DELETE CASCADE
    CHECK (min_wavelength <= max_wavelength)
    CONSTRAINT fpu_slit_or_other
      CHECK ((fpu_slit IS     NULL AND fpu_other IS NOT NULL) OR
             (fpu_slit IS NOT NULL AND fpu_other IS     NULL))
);

ALTER TABLE smart_gnirs OWNER TO postgres;

COMMENT ON COLUMN smart_gnirs.min_wavelength IS 'Å';
COMMENT ON COLUMN smart_gnirs.max_wavelength IS 'Å';

-- While working on GNIRS we found out GMOS used angstroms too
COMMENT ON COLUMN smart_gmos_south.min_wavelength IS 'Å';
COMMENT ON COLUMN smart_gmos_south.max_wavelength IS 'Å';
COMMENT ON COLUMN smart_gmos_north.min_wavelength IS 'Å';
COMMENT ON COLUMN smart_gmos_north.max_wavelength IS 'Å';
