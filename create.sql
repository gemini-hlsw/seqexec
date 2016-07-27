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
    program_id character varying(32) NOT NULL,
    observation_id character varying(40) NOT NULL,
    observation_index smallint NOT NULL,
    metadata_stepcount smallint NOT NULL,
    metadata_complete boolean NOT NULL,
    obs_class_id character varying(20),
    "observe_dataLabel" character varying(64),
    observe_object character varying(255),
    "observe_observeType" character varying(20),
    "observe_sciBand" smallint,
    instrument_instrument character varying(20)
);


ALTER TABLE step OWNER TO postgres;

--
-- Data for Name: charge_class; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY charge_class (charge_class_id, name) FROM stdin;
noncharged	Non-charged
partner	Partner
program	Program
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

COPY step (program_id, observation_id, observation_index, metadata_stepcount, metadata_complete, obs_class_id, "observe_dataLabel", observe_object, "observe_observeType", "observe_sciBand", instrument_instrument) FROM stdin;
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
-- Name: pk_observation; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step
    ADD CONSTRAINT pk_observation PRIMARY KEY (metadata_stepcount, observation_id);


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
-- Name: ix_observation_instrument; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_observation_instrument ON observation USING btree (instrument);


--
-- Name: ix_observation_program_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_observation_program_id ON observation USING btree (program_id);


--
-- Name: ix_step_observation_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_step_observation_id ON step USING btree (observation_id);


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
-- Name: fk_step_observation; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step
    ADD CONSTRAINT fk_step_observation FOREIGN KEY (observation_id, instrument_instrument) REFERENCES observation(observation_id, instrument) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: program_program_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY program
    ADD CONSTRAINT program_program_type_id_fkey FOREIGN KEY (program_type_id) REFERENCES program_type(program_type_id);


--
-- Name: step_obs_class_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step
    ADD CONSTRAINT step_obs_class_id_fkey FOREIGN KEY (obs_class_id) REFERENCES obs_class(obs_class_id);


--
-- Name: step_observation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY step
    ADD CONSTRAINT step_observation_id_fkey FOREIGN KEY (observation_id) REFERENCES observation(observation_id) ON UPDATE CASCADE ON DELETE CASCADE;


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

