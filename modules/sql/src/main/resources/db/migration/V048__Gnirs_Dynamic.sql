--
-- Name: step_gnirs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_gnirs (
    step_gnirs_id         integer      PRIMARY KEY REFERENCES step,
    camera                identifier   NOT NULL    REFERENCES e_gnirs_camera,
    decker                identifier   NOT NULL    REFERENCES e_gnirs_decker,
    disperser             identifier   NOT NULL    REFERENCES e_gnirs_disperser,
    disperser_order       identifier   NOT NULL    REFERENCES e_gnirs_disperser_order,
    exposure_time         milliseconds NOT NULL,
    filter                identifier   NOT NULL    REFERENCES e_gnirs_filter,
    fpu_slit              identifier               REFERENCES e_gnirs_fpu_slit,
    fpu_other             identifier               REFERENCES e_gnirs_fpu_other,
    prism                 identifier   NOT NULL    REFERENCES e_gnirs_prism,
    read_mode             identifier   NOT NULL    REFERENCES e_gnirs_read_mode
    CONSTRAINT fpu_slit_or_other
      CHECK ((fpu_slit IS     NULL AND fpu_other IS NOT NULL) OR
             (fpu_slit IS NOT NULL AND fpu_other IS     NULL))
);

ALTER TABLE step_gnirs OWNER TO postgres;
