--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.3
-- Dumped by pg_dump version 9.5.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: f2_decker; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE f2_decker AS ENUM (
    'Imaging',
    'Long Slit',
    'MOS'
);


ALTER TYPE f2_decker OWNER TO postgres;

--
-- Name: gcal_lamp_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE gcal_lamp_type AS ENUM (
    'arc',
    'flat'
);


ALTER TYPE gcal_lamp_type OWNER TO postgres;

--
-- Name: gcal_shutter; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE gcal_shutter AS ENUM (
    'Open',
    'Closed'
);


ALTER TYPE gcal_shutter OWNER TO postgres;

--
-- Name: step_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE step_type AS ENUM (
    'Bias',
    'Dark',
    'Gcal',
    'Science',
    'Smart'
);


ALTER TYPE step_type OWNER TO postgres;

--
-- Name: target_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE target_type AS ENUM (
    'sidereal',
    'nonsidereal'
);


ALTER TYPE target_type OWNER TO postgres;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: charge_class; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE charge_class (
    charge_class_id character varying NOT NULL,
    name character varying
);


ALTER TABLE charge_class OWNER TO postgres;

--
-- Name: f2_disperser; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE f2_disperser (
    tag character varying(32) NOT NULL,
    log_value character varying(20) NOT NULL,
    wavelength double precision
);


ALTER TABLE f2_disperser OWNER TO postgres;

--
-- Name: f2_filter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE f2_filter (
    tag character varying(32) NOT NULL,
    log_value character varying(20),
    wavelength double precision
);


ALTER TABLE f2_filter OWNER TO postgres;

--
-- Name: f2_fpunit; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE f2_fpunit (
    tag character varying(20) NOT NULL,
    log_value character varying(20) NOT NULL,
    slit_width smallint NOT NULL,
    decker f2_decker NOT NULL
);


ALTER TABLE f2_fpunit OWNER TO postgres;

--
-- Name: f2_lyot_wheel; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE f2_lyot_wheel (
    tag character varying(32) NOT NULL,
    log_value character varying(20) NOT NULL,
    plate_scale double precision NOT NULL,
    pixel_scale double precision NOT NULL,
    obsolete boolean NOT NULL
);


ALTER TABLE f2_lyot_wheel OWNER TO postgres;

--
-- Name: gcal_filter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE gcal_filter (
    gcal_filter_id character varying(20) NOT NULL,
    title character varying(20) NOT NULL,
    obsolete boolean NOT NULL
);


ALTER TABLE gcal_filter OWNER TO postgres;

--
-- Name: gcal_lamp; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE gcal_lamp (
    gcal_lamp_id character varying(20) NOT NULL,
    "tccName" character varying(20) NOT NULL,
    lamp_type gcal_lamp_type NOT NULL
);


ALTER TABLE gcal_lamp OWNER TO postgres;

--
-- Name: instrument; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE instrument (
    instrument_id character varying(20) NOT NULL,
    name character varying(20) NOT NULL
);


ALTER TABLE instrument OWNER TO postgres;

--
-- Name: obs_class; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE obs_class (
    obs_class_id character varying(20) NOT NULL,
    name character varying(20) NOT NULL,
    priority smallint NOT NULL,
    charge_class_id character varying NOT NULL,
    log_value character varying(10) NOT NULL
);


ALTER TABLE obs_class OWNER TO postgres;

--
-- Name: observation; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE observation (
    program_id character varying(32) NOT NULL,
    observation_index smallint NOT NULL,
    title character varying(255),
    observation_id character varying(40) NOT NULL,
    instrument character varying(20),
    CONSTRAINT observation_id_check CHECK (((observation_id)::text = (((program_id)::text || '-'::text) || observation_index)))
);


ALTER TABLE observation OWNER TO postgres;

--
-- Name: program; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE program (
    program_id character varying(32) NOT NULL,
    semester_id character varying(20),
    site_id character varying(2),
    program_type_id character varying,
    index smallint,
    day date,
    title character varying(255) DEFAULT '«Untitled»'::character varying NOT NULL
);


ALTER TABLE program OWNER TO postgres;

--
-- Name: TABLE program; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE program IS 'TODO: constraint that requires structured data to be consistent with the program id, when present';


--
-- Name: program_type; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE program_type (
    name character varying(20) NOT NULL,
    program_type_id character varying(3) NOT NULL
);


ALTER TABLE program_type OWNER TO postgres;

--
-- Name: TABLE program_type; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE program_type IS 'TODO: constraint forbidding more rows';


--
-- Name: semester; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE semester (
    semester_id character varying(20) NOT NULL,
    year smallint NOT NULL,
    half character(1) NOT NULL,
    CONSTRAINT check_semester_id CHECK (((semester_id)::text = (year || (half)::text)))
);


ALTER TABLE semester OWNER TO postgres;

--
-- Name: TABLE semester; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE semester IS '// TODO: start/end dates for site';


--
-- Name: site; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE site (
    site_id character varying(2) NOT NULL,
    name character varying(20) NOT NULL,
    mountain character varying(20) NOT NULL,
    longitude real NOT NULL,
    latitude real NOT NULL,
    altitude smallint NOT NULL,
    timezone character varying(20) NOT NULL,
    CONSTRAINT site_id_check CHECK ((((site_id)::text = 'GS'::text) OR ((site_id)::text = 'GN'::text)))
);


ALTER TABLE site OWNER TO postgres;

--
-- Name: TABLE site; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE site IS 'Lookup table for site information.';


--
-- Name: COLUMN site.altitude; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN site.altitude IS 'Altitude in meters.';


--
-- Name: step; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step (
    observation_id character varying(40) NOT NULL,
    index smallint NOT NULL,
    instrument_id character varying(20) NOT NULL,
    step_type step_type NOT NULL
);


ALTER TABLE step OWNER TO postgres;

--
-- Name: step_bias; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_bias (
    index smallint NOT NULL,
    observation_id character varying(40) NOT NULL
);


ALTER TABLE step_bias OWNER TO postgres;

--
-- Name: step_dark; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_dark (
    index smallint NOT NULL,
    observation_id character varying(40) NOT NULL
);


ALTER TABLE step_dark OWNER TO postgres;

--
-- Name: step_gcal; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_gcal (
    index smallint NOT NULL,
    observation_id character varying(40) NOT NULL,
    gcal_lamp_id character varying,
    shutter gcal_shutter NOT NULL
);


ALTER TABLE step_gcal OWNER TO postgres;

--
-- Name: step_science; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_science (
    index smallint NOT NULL,
    observation_id character varying(40) NOT NULL,
    offset_p double precision NOT NULL,
    offset_q double precision NOT NULL
);


ALTER TABLE step_science OWNER TO postgres;

--
-- Data for Name: charge_class; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY charge_class (charge_class_id, name) FROM stdin;
noncharged	Non-charged
partner	Partner
program	Program
\.


--
-- Data for Name: f2_disperser; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY f2_disperser (tag, log_value, wavelength) FROM stdin;
None	none	\N
R=1200 (J + H) grism	R1200JH	1.3899999999999999
R=1200 (H + K) grism	R1200HK	1.871
R=3000 (J or H or K) grism	R3000	1.64999999999999991
\.


--
-- Data for Name: f2_filter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY f2_filter (tag, log_value, wavelength) FROM stdin;
Open	Open	1.60000000000000009
Y (1.02 um)	Y	1.02000000000000002
F1056 (1.056 um)	F1056	1.05600000000000005
F1063 (1.063 um)	F1063	1.06299999999999994
J-low (1.15 um)	J-low	1.14999999999999991
J (1.25 um)	J	1.25
H (1.65 um)	H	1.64999999999999991
K-long (2.20 um)	K-long	2.20000000000000018
K-short (2.15 um)	K-short	2.14999999999999991
JH (spectroscopic)	JH	1.3899999999999999
HK (spectroscopic)	HK	1.871
\.


--
-- Data for Name: f2_fpunit; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY f2_fpunit (tag, log_value, slit_width, decker) FROM stdin;
Imaging (none)	none	0	Imaging
1-pix longslit	longslit_1	1	Long Slit
2-pix longslit	longslit_2	2	Long Slit
3-pix longslit	longslit_3	3	Long Slit
4-pix longslit	longslit_4	4	Long Slit
6-pix longslit	longslit_6	6	Long Slit
8-pix longslit	longslit_8	8	Long Slit
2-pix pinhole grid	pinhole	0	Imaging
subpix pinhole grid	subpixPinhole	0	Imaging
Custom Mask	custom	0	MOS
\.


--
-- Data for Name: f2_lyot_wheel; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY f2_lyot_wheel (tag, log_value, plate_scale, pixel_scale, obsolete) FROM stdin;
f/16 (open)	f/16	1.6100000000000001	0.179999999999999993	f
f/32 MCAO high background	f/32 high	0.805000000000000049	0.0899999999999999967	t
f/32 MCAO low background	f/32 low	0.805000000000000049	0.0899999999999999967	t
f/33 (Gems)	f/33 Gems	0.78400000000000003	0.0899999999999999967	t
f/33 (GeMS under-sized)	GeMS under	0.78400000000000003	0.0899999999999999967	f
f/33 (GeMS over-sized)	GeMS over	0.78400000000000003	0.0899999999999999967	f
Hartmann A (H1)	Hartmann A (H1)	0	0	f
Hartmann B (H2)	Hartmann B (H2)	0	0	f
\.


--
-- Data for Name: gcal_filter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY gcal_filter (gcal_filter_id, title, obsolete) FROM stdin;
NONE	none	f
ND_10	ND1.0	f
ND_16	ND1.6	t
ND_20	ND2.0	f
ND_30	ND3.0	f
ND_40	ND4.0	f
ND_45	ND4-5	f
ND_50	ND5.0	t
GMOS	GMOS balance	f
HROS	HROS balance	t
NIR	NIR balance	f
\.


--
-- Data for Name: gcal_lamp; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY gcal_lamp (gcal_lamp_id, "tccName", lamp_type) FROM stdin;
IR grey body - high	GCALflat	flat
IR grey body - low	GCALflat	flat
Quartz Halogen	GCALflat	flat
Ar arc	Ar	arc
ThAr arc	ThAr	arc
CuAr arc	CuAr	arc
Xe arc	Xe	arc
\.


--
-- Data for Name: instrument; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY instrument (instrument_id, name) FROM stdin;
GNIRS	GNIRS
NIRI	NIRI
Phoenix	Phoenix
TReCS	TReCS
Flamingos2	Flamingos2
NICI	NICI
bHROS	bHROS
Visitor Instrument	Visitor Instrument
Michelle	Michelle
GMOS-S	GMOS-S
AcqCam	AcqCam
GMOS-N	GMOS-N
NIFS	NIFS
GPI	GPI
GSAOI	GSAOI
\.


--
-- Data for Name: obs_class; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY obs_class (obs_class_id, name, priority, charge_class_id, log_value) FROM stdin;
science	Science	0	program	SCI
progCal	Nighttime Program Ca	1	program	NCAL
partnerCal	Nighttime Partner Ca	2	partner	PCAL
acq	Acquisition	3	program	ACQ
acqCal	Acquisition Calibrat	4	partner	ACAL
dayCal	Daytime Calibration	5	noncharged	DCAL
\.


--
-- Data for Name: observation; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY observation (program_id, observation_index, title, observation_id, instrument) FROM stdin;
\.


--
-- Data for Name: program; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY program (program_id, semester_id, site_id, program_type_id, index, day, title) FROM stdin;
\.


--
-- Data for Name: program_type; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY program_type (name, program_type_id) FROM stdin;
Calibration	CAL
Classical	C
Demo Science	DS
Director's Time	DD
Engineering	ENG
Fast Turnaround	FT
Large Program	LP
Queue	Q
System Verification	SV
\.


--
-- Data for Name: semester; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY semester (semester_id, year, half) FROM stdin;
2006A	2006	A
2006B	2006	B
2007B	2007	B
2012B	2012	B
2003A	2003	A
2003B	2003	B
2004A	2004	A
\.


--
-- Data for Name: site; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY site (site_id, name, mountain, longitude, latitude, altitude, timezone) FROM stdin;
GN	Gemini North	Mauna Kea	-155.469055	19.8238068	4213	Pacific/Honolulu
GS	Gemini South	Cerro Pachon	-70.7366867	-30.2407494	2722	America/Santiago
\.


--
-- Data for Name: step; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY step (observation_id, index, instrument_id, step_type) FROM stdin;
\.


--
-- Data for Name: step_bias; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY step_bias (index, observation_id) FROM stdin;
\.


--
-- Data for Name: step_dark; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY step_dark (index, observation_id) FROM stdin;
\.


--
-- Data for Name: step_gcal; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY step_gcal (index, observation_id, gcal_lamp_id, shutter) FROM stdin;
\.


--
-- Data for Name: step_science; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY step_science (index, observation_id, offset_p, offset_q) FROM stdin;
\.


--
-- Name: charge_test_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY charge_class
    ADD CONSTRAINT charge_test_name_key UNIQUE (name);


--
-- Name: charge_test_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY charge_class
    ADD CONSTRAINT charge_test_pkey PRIMARY KEY (charge_class_id);


--
-- Name: f2_disperser_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY f2_disperser
    ADD CONSTRAINT f2_disperser_pkey PRIMARY KEY (tag);


--
-- Name: f2_filter_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY f2_filter
    ADD CONSTRAINT f2_filter_pkey PRIMARY KEY (tag);


--
-- Name: f2_lyot_wheel_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY f2_lyot_wheel
    ADD CONSTRAINT f2_lyot_wheel_pkey PRIMARY KEY (tag);


--
-- Name: gcal_filter_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY gcal_filter
    ADD CONSTRAINT gcal_filter_pkey PRIMARY KEY (gcal_filter_id);


--
-- Name: gcal_lamp_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY gcal_lamp
    ADD CONSTRAINT gcal_lamp_pkey PRIMARY KEY (gcal_lamp_id);


--
-- Name: instrument_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY instrument
    ADD CONSTRAINT instrument_pkey PRIMARY KEY (instrument_id);


--
-- Name: obs_class_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY obs_class
    ADD CONSTRAINT obs_class_pkey PRIMARY KEY (obs_class_id);


--
-- Name: observation_instrument_observation_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY observation
    ADD CONSTRAINT observation_instrument_observation_id_key UNIQUE (instrument, observation_id);


--
-- Name: observation_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY observation
    ADD CONSTRAINT observation_pkey PRIMARY KEY (observation_id);


--
-- Name: program_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY program
    ADD CONSTRAINT program_pkey PRIMARY KEY (program_id);


--
-- Name: program_type_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY program_type
    ADD CONSTRAINT program_type_pkey PRIMARY KEY (program_type_id);


--
-- Name: semester_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY semester
    ADD CONSTRAINT semester_pkey PRIMARY KEY (semester_id);


--
-- Name: site_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY site
    ADD CONSTRAINT site_pkey PRIMARY KEY (site_id);


--
-- Name: step_bias_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step_bias
    ADD CONSTRAINT step_bias_pkey PRIMARY KEY (index, observation_id);


--
-- Name: step_dark_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step_dark
    ADD CONSTRAINT step_dark_pkey PRIMARY KEY (index, observation_id);


--
-- Name: step_gcal_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step_gcal
    ADD CONSTRAINT step_gcal_pkey PRIMARY KEY (index, observation_id);


--
-- Name: step_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step
    ADD CONSTRAINT step_pkey PRIMARY KEY (index, observation_id);


--
-- Name: ix_observation_instrument; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_observation_instrument ON observation USING btree (instrument);


--
-- Name: ix_observation_program_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_observation_program_id ON observation USING btree (program_id);


--
-- Name: FK_obs_class_1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY obs_class
    ADD CONSTRAINT "FK_obs_class_1" FOREIGN KEY (charge_class_id) REFERENCES charge_class(charge_class_id);


--
-- Name: FK_observation_1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY observation
    ADD CONSTRAINT "FK_observation_1" FOREIGN KEY (program_id) REFERENCES program(program_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: FK_program_1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY program
    ADD CONSTRAINT "FK_program_1" FOREIGN KEY (semester_id) REFERENCES semester(semester_id);


--
-- Name: FK_program_2; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY program
    ADD CONSTRAINT "FK_program_2" FOREIGN KEY (site_id) REFERENCES site(site_id);


--
-- Name: program_program_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY program
    ADD CONSTRAINT program_program_type_id_fkey FOREIGN KEY (program_type_id) REFERENCES program_type(program_type_id);


--
-- Name: step_bias_index_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step_bias
    ADD CONSTRAINT step_bias_index_fkey FOREIGN KEY (index, observation_id) REFERENCES step(index, observation_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: step_dark_index_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step_dark
    ADD CONSTRAINT step_dark_index_fkey FOREIGN KEY (index, observation_id) REFERENCES step(index, observation_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: step_gcal_gcal_lamp_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step_gcal
    ADD CONSTRAINT step_gcal_gcal_lamp_id_fkey FOREIGN KEY (gcal_lamp_id) REFERENCES gcal_lamp(gcal_lamp_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: step_gcal_index_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step_gcal
    ADD CONSTRAINT step_gcal_index_fkey FOREIGN KEY (index, observation_id) REFERENCES step(index, observation_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: step_instrument_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step
    ADD CONSTRAINT step_instrument_id_fkey FOREIGN KEY (instrument_id) REFERENCES instrument(instrument_id);


--
-- Name: step_observation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step
    ADD CONSTRAINT step_observation_id_fkey FOREIGN KEY (observation_id) REFERENCES observation(observation_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: step_science_index_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step_science
    ADD CONSTRAINT step_science_index_fkey FOREIGN KEY (index, observation_id) REFERENCES step(index, observation_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO rnorris;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

