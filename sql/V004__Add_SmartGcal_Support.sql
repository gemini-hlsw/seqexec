--
-- SmartGcal Support
--

--
-- Name: baseline; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE gcal_baseline_type AS ENUM (
    'Day',
    'Night'
);


ALTER TYPE gcal_baseline_type OWNER TO postgres;

--
-- Name: gcal_lamp; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE gcal_lamp_type AS ENUM (
    'Arc',
    'Flat'
);


ALTER TYPE gcal_lamp_type OWNER TO postgres;


--
-- Name: smart_f2; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE smart_f2 (
  lamp      gcal_lamp_type     NOT NULL,
  baseline  gcal_baseline_type NOT NULL,
  disperser identifier         NOT NULL REFERENCES e_f2_disperser ON DELETE CASCADE,
  filter    identifier         NOT NULL REFERENCES e_f2_filter    ON DELETE CASCADE,
  fpu       identifier         NOT NULL REFERENCES e_f2_fpunit    ON DELETE CASCADE,
  gcal_id   integer            NOT NULL REFERENCES gcal           ON DELETE CASCADE
);

ALTER TABLE smart_f2 OWNER TO postgres;



