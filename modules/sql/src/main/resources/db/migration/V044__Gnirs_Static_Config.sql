--
-- Name: static_gnirs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE static_gnirs (
    program_id        text       NOT NULL,
    instrument        identifier NOT NULL,
    observation_index id_index   NOT NULL,
    well_depth        identifier NOT NULL REFERENCES e_gnirs_well_depth,
    PRIMARY KEY (program_id, observation_index, instrument),
    CONSTRAINT  observation_ref FOREIGN KEY (program_id, observation_index, instrument) REFERENCES observation ON DELETE CASCADE,
    CONSTRAINT is_gnirs CHECK (instrument = 'Gnirs')
);

ALTER TABLE static_gnirs OWNER TO postgres;
