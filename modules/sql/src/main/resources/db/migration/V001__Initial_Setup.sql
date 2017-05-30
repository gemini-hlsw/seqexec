--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.4
-- Dumped by pg_dump version 9.5.4

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


--
-- Name: tsm_system_time; Type: EXTENSION; Schema: -; Owner:
--

CREATE EXTENSION IF NOT EXISTS tsm_system_time WITH SCHEMA public;


--
-- Name: EXTENSION tsm_system_time; Type: COMMENT; Schema: -; Owner:
--

COMMENT ON EXTENSION tsm_system_time IS 'TABLESAMPLE method which accepts time in milliseconds as a limit';


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
-- Name: identifier; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN identifier AS character varying(32)
	CONSTRAINT identifier_check CHECK (((VALUE)::text ~ '^[A-Z][A-Za-z0-9_]*$'::text));


ALTER DOMAIN identifier OWNER TO postgres;


--
-- Name: coadds; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN coadds AS smallint
    DEFAULT    1
    CONSTRAINT coadds_check CHECK (VALUE > 0);

ALTER DOMAIN coadds OWNER TO postgres;


--
-- Name: milliseconds; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN milliseconds AS integer
    DEFAULT    0
    CONSTRAINT miliseconds_check CHECK (VALUE >= 0);

ALTER DOMAIN milliseconds OWNER TO postgres;


--
-- Name: index; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN id_index AS smallint
    DEFAULT    1
    CONSTRAINT id_index_check CHECK (VALUE > 0);

ALTER DOMAIN id_index OWNER TO postgres;


--
-- Name: log_level; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE log_level AS ENUM (
    'FINEST',
    'FINER',
    'FINE',
    'CONFIG',
    'INFO',
    'WARNING',
    'SEVERE'
);


ALTER TYPE log_level OWNER TO postgres;

--
-- Name: step_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE step_type AS ENUM (
    'Bias',
    'Dark',
    'Gcal',
    'Science',
    'SmartGcal'
);


ALTER TYPE step_type OWNER TO postgres;

--
-- Name: smart_gcal_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE smart_gcal_type AS ENUM (
    'Arc',
    'Flat',
    'DayBaseline',
    'NightBaseline'
);

ALTER TYPE smart_gcal_type OWNER TO postgres;


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
-- Name: e_f2_disperser; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_f2_disperser (
    id          identifier            PRIMARY KEY,
    wavelength  double precision,
    short_name  character varying(20) NOT NULL,
    long_name   character varying(64) NOT NULL
);


ALTER TABLE e_f2_disperser OWNER TO postgres;

--
-- Name: e_f2_filter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_f2_filter (
    id         identifier            PRIMARY KEY,
    wavelength double precision,
    short_name character varying(20) NOT NULL,
    long_name  character varying(20) NOT NULL,
    obsolete   boolean               NOT NULL
);


ALTER TABLE e_f2_filter OWNER TO postgres;

--
-- Name: e_f2_fpunit; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_f2_fpunit (
    id         identifier            PRIMARY KEY,
    short_name character varying(20) NOT NULL,
    slit_width smallint              NOT NULL,
    decker     f2_decker             NOT NULL,
    long_name  character varying(20) NOT NULL,
    obsolete   boolean               NOT NULL
);


ALTER TABLE e_f2_fpunit OWNER TO postgres;

--
-- Name: e_f2_lyot_wheel; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_f2_lyot_wheel (
    id          identifier            PRIMARY KEY,
    short_name  character varying(20) NOT NULL,
    plate_scale double precision      NOT NULL,
    pixel_scale double precision      NOT NULL,
    obsolete    boolean               NOT NULL,
    long_name   character varying(32) NOT NULL
);


ALTER TABLE e_f2_lyot_wheel OWNER TO postgres;


--
-- Name: e_f2_read_mode; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_f2_read_mode (
    id                        identifier            PRIMARY KEY,
    short_name                character varying(8)  NOT NULL,
    long_name                 character varying(20) NOT NULL,
    description               character varying(20) NOT NULL,
    minimum_exposure_time     milliseconds          NOT NULL,
    recommended_exposure_time milliseconds          NOT NULL,
    readout_time              milliseconds          NOT NULL,
    read_count                smallint              NOT NULL,
    read_noise                double precision      NOT NULL
);


ALTER TABLE e_f2_read_mode OWNER TO postgres;


--
-- Name: e_f2_window_cover; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_f2_window_cover (
    id         identifier            PRIMARY KEY,
    short_name character varying(20) NOT NULL,
    long_name  character varying(20) NOT NULL
);


ALTER TABLE e_f2_window_cover OWNER TO postgres;

--
-- Name: e_gcal_filter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gcal_filter (
    id         character varying(20) PRIMARY KEY,
    short_name character varying(20) NOT NULL,
    obsolete   boolean               NOT NULL,
    long_name  character varying(20) NOT NULL
);


ALTER TABLE e_gcal_filter OWNER TO postgres;

--
-- Name: e_gcal_continuum; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gcal_continuum (
    id         character varying(20) PRIMARY KEY,
    short_name character varying(20) NOT NULL,
    long_name  character varying(20) NOT NULL,
    obsolete   boolean               NOT NULL
);


ALTER TABLE e_gcal_continuum OWNER TO postgres;

--
-- Name: e_gcal_arc; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gcal_arc (
    id         character varying(20) PRIMARY KEY,
    short_name character varying(20) NOT NULL,
    long_name  character varying(20) NOT NULL,
    obsolete   boolean               NOT NULL
);


ALTER TABLE e_gcal_arc OWNER TO postgres;

--
-- Name: e_gcal_diffuser; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gcal_diffuser (
    id         character varying(20) PRIMARY KEY,
    short_name character varying(20) NOT NULL,
    obsolete   boolean               NOT NULL,
    long_name  character varying(20) NOT NULL
);

ALTER TABLE e_gcal_diffuser OWNER TO postgres;

--
-- Name: e_gcal_shutter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gcal_shutter (
    id         character varying(20) PRIMARY KEY,
    short_name character varying(20) NOT NULL,
    obsolete   boolean               NOT NULL,
    long_name  character varying(20) NOT NULL
);

ALTER TABLE e_gcal_shutter OWNER TO postgres;

--
-- Name: e_instrument; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_instrument (
    id identifier PRIMARY KEY,
    short_name character varying(20) NOT NULL,
    long_name character varying(64) NOT NULL,
    obsolete boolean NOT NULL
);


ALTER TABLE e_instrument OWNER TO postgres;

--
-- Name: e_program_role; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_program_role (
    id identifier NOT NULL,
    short_name character varying(32),
    long_name character varying(64)
);


ALTER TABLE e_program_role OWNER TO postgres;

--
-- Name: e_program_type; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_program_type (
    id identifier NOT NULL,
    short_name character varying(32) NOT NULL,
    long_name character varying(64) NOT NULL,
    obsolete boolean NOT NULL
);


ALTER TABLE e_program_type OWNER TO postgres;

--
-- Name: COLUMN e_program_type.id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_program_type.id IS 'enum identifier';


--
-- Name: COLUMN e_program_type.short_name; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_program_type.short_name IS 'short name, ''SV''';


--
-- Name: COLUMN e_program_type.long_name; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_program_type.long_name IS 'long name, ''Classical''';


--
-- Name: COLUMN e_program_type.obsolete; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_program_type.obsolete IS 'true if no longer available';


--
-- Name: e_site; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_site (
    id identifier NOT NULL,
    long_name character varying(20) NOT NULL,
    mountain character varying(20) NOT NULL,
    longitude real NOT NULL,
    latitude real NOT NULL,
    altitude smallint NOT NULL,
    timezone character varying(20) NOT NULL,
    short_name character varying(20) NOT NULL
);


ALTER TABLE e_site OWNER TO postgres;

--
-- Name: TABLE e_site; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE e_site IS 'Lookup table for site information.';


--
-- Name: COLUMN e_site.id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_site.id IS 'enum identifier';


--
-- Name: COLUMN e_site.long_name; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_site.long_name IS 'long name, ''Gemini South''';


--
-- Name: COLUMN e_site.mountain; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_site.mountain IS 'name of mountain, ''Mauna Kea''';


--
-- Name: COLUMN e_site.longitude; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_site.longitude IS 'longitude in degrees';


--
-- Name: COLUMN e_site.latitude; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_site.latitude IS 'latitude in degrees';


--
-- Name: COLUMN e_site.altitude; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_site.altitude IS 'altitude in meters';


--
-- Name: COLUMN e_site.timezone; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_site.timezone IS 'key for java.util.TimeZone.getTimeZone';


--
-- Name: COLUMN e_site.short_name; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN e_site.short_name IS 'short name, ''GS''';


--
-- Name: e_template; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_template (
    id identifier NOT NULL,
    short_name character varying(32) NOT NULL,
    long_name character varying(64) NOT NULL,
    obsolete boolean NOT NULL
);


ALTER TABLE e_template OWNER TO postgres;

--
-- Name: gem_user; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE gem_user (
    id character varying(20) NOT NULL,
    first character varying(20) NOT NULL,
    last character varying(20) NOT NULL,
    md5 character(32) NOT NULL,
    email character varying(40) NOT NULL,
    staff boolean DEFAULT false NOT NULL
);


ALTER TABLE gem_user OWNER TO postgres;

--
-- Name: gem_user_program; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE gem_user_program (
    user_id character varying(20) NOT NULL,
    program_id character varying(32) NOT NULL,
    program_role identifier NOT NULL
);


ALTER TABLE gem_user_program OWNER TO postgres;

--
-- Name: log; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE log (
    "timestamp" timestamp(5) with time zone NOT NULL,
    level log_level NOT NULL,
    program character varying(32),
    message character varying(255) NOT NULL,
    stacktrace text,
    elapsed bigint,
    user_id character varying
);


ALTER TABLE log OWNER TO postgres;

--
-- Name: observation; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE observation (
    program_id character varying(32) NOT NULL,
    observation_index id_index NOT NULL,
    title character varying(255),
    observation_id character varying(40) PRIMARY KEY,
    instrument identifier,
    CONSTRAINT observation_id_check CHECK (((observation_id)::text = (((program_id)::text || '-'::text) || observation_index)))
);


ALTER TABLE observation OWNER TO postgres;

--
-- Name: program; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE program (
    program_id character varying(32) NOT NULL,
    semester_id character varying(20),
    site identifier,
    program_type identifier,
    index id_index,
    day date,
    title character varying(255) DEFAULT '«Untitled»'::character varying NOT NULL
);


ALTER TABLE program OWNER TO postgres;

--
-- Name: TABLE program; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE program IS 'TODO: constraint that requires structured data to be consistent with the program id, when present';


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
-- Name: step; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step (
    step_id        SERIAL                PRIMARY KEY,
    observation_id character varying(40) NOT NULL REFERENCES observation(observation_id) ON UPDATE CASCADE ON DELETE CASCADE,
    location       integer[]             NOT NULL,
    instrument     identifier            NOT NULL REFERENCES e_instrument,
    step_type      step_type             NOT NULL,
    UNIQUE (observation_id, location)
);

ALTER TABLE step OWNER TO postgres;

--
-- Name: step_bias; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_bias (
    step_bias_id integer PRIMARY KEY REFERENCES step ON DELETE CASCADE
);


ALTER TABLE step_bias OWNER TO postgres;

--
-- Name: step_dark; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_dark (
    step_dark_id integer PRIMARY KEY REFERENCES step ON DELETE CASCADE
);


ALTER TABLE step_dark OWNER TO postgres;

--
-- Name: step_f2; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_f2 (
    step_f2_id     integer       PRIMARY KEY REFERENCES step              ON DELETE CASCADE,
    disperser      identifier    NOT NULL    REFERENCES e_f2_disperser    ON DELETE CASCADE,
    exposure_time  milliseconds  NOT NULL,
    filter         identifier    NOT NULL    REFERENCES e_f2_filter       ON DELETE CASCADE,
    fpu            identifier    NOT NULL    REFERENCES e_f2_fpunit       ON DELETE CASCADE,
    lyot_wheel     identifier    NOT NULL    REFERENCES e_f2_lyot_wheel   ON DELETE CASCADE,
    mos_preimaging boolean       NOT NULL,
    read_mode      identifier    NOT NULL    REFERENCES e_f2_read_mode    ON DELETE CASCADE,
    window_cover   identifier    NOT NULL    REFERENCES e_f2_window_cover ON DELETE CASCADE
);


ALTER TABLE step_f2 OWNER TO postgres;


--
-- Name: step_gcal; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE gcal (
    gcal_id       SERIAL       PRIMARY KEY,
    step_id       integer                             REFERENCES step             ON DELETE CASCADE,
    continuum     identifier                          REFERENCES e_gcal_continuum ON DELETE CASCADE,
    ar_arc        boolean      NOT NULL DEFAULT FALSE,
    cuar_arc      boolean      NOT NULL DEFAULT FALSE,
    thar_arc      boolean      NOT NULL DEFAULT FALSE,
    xe_arc        boolean      NOT NULL DEFAULT FALSE,
    filter        identifier   NOT NULL                REFERENCES e_gcal_filter    ON DELETE CASCADE,
    diffuser      identifier   NOT NULL                REFERENCES e_gcal_diffuser  ON DELETE CASCADE,
    shutter       identifier   NOT NULL                REFERENCES e_gcal_shutter   ON DELETE CASCADE,
    exposure_time milliseconds NOT NULL,
    coadds        coadds       NOT NULL,
    CONSTRAINT check_lamp CHECK ((continuum IS NULL) = (ar_arc OR cuar_arc OR thar_arc OR xe_arc))
);


ALTER TABLE gcal OWNER TO postgres;


--
-- Name: step_gcal; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_gcal (
    step_gcal_id  integer PRIMARY KEY REFERENCES step ON DELETE CASCADE,
    gcal_id       integer NOT NULL    REFERENCES gcal ON DELETE CASCADE
);


ALTER TABLE step_gcal OWNER TO postgres;

--
-- Name: step_gcal; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_smart_gcal (
    step_smart_gcal_id integer PRIMARY KEY REFERENCES step ON DELETE CASCADE,
    type               smart_gcal_type NOT NULL
);


ALTER TABLE step_smart_gcal OWNER TO postgres;

--
-- Name: step_science; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE step_science (
    step_science_id integer      PRIMARY KEY REFERENCES step ON DELETE CASCADE,
    offset_p        numeric(9,3) NOT NULL,
    offset_q        numeric(9,3) NOT NULL
);


ALTER TABLE step_science OWNER TO postgres;


--
-- Data for Name: dataset; Type: TABLE DATA; Schema: public; Owner: postgres
--

CREATE TABLE dataset (
    dataset_label   character varying(46)        PRIMARY KEY,
    observation_id  character varying(40)        REFERENCES observation ON DELETE CASCADE,
    dataset_index   id_index                     NOT NULL,
    step_id         integer                      REFERENCES step        ON DELETE CASCADE,
    filename        character varying(24)        NOT NULL,
    "timestamp"     timestamp (5) WITH TIME ZONE NOT NULL
    CONSTRAINT dataset_id_check CHECK ((dataset_label)::text = (((observation_id)::text || '-'::text) || trim(leading from to_char(dataset_index, '9000'))))
);



--
-- Data for Name: e_f2_disperser; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_f2_disperser (id, wavelength, short_name, long_name) FROM stdin;
R1200JH	1.3899999999999999	R1200JH	R=1200 (J + H) grism
R1200HK	1.871	R1200HK	R=1200 (H + K) grism
R3000	1.64999999999999991	R3000	R=3000 (J or H or K) grism
NoDisperser	\N	None	None
\.


--
-- Data for Name: e_f2_filter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_f2_filter (id, wavelength, short_name, long_name, obsolete) FROM stdin;
Open	1.60000000000000009	Open	Open	f
Y	1.02000000000000002	Y	Y (1.02 um)	f
F1056	1.05600000000000005	F1056	F1056 (1.056 um)	f
J	1.25	J	J (1.25 um)	f
H	1.64999999999999991	H	H (1.65 um)	f
JH	1.3899999999999999	JH	JH (spectroscopic)	f
HK	1.871	HK	HK (spectroscopic)	f
JLow	1.14999999999999991	J-low	J-low (1.15 um)	f
KLong	2.20000000000000018	K-long	K-long (2.20 um)	f
KShort	2.14999999999999991	K-short	K-short (2.15 um)	f
F1063	1.06299999999999994	F1063	F1063 (1.063 um)	f
Dark	\N	Dark	Dark	f
\.


--
-- Data for Name: e_f2_fpunit; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_f2_fpunit (id, short_name, slit_width, decker, long_name, obsolete) FROM stdin;
Pinhole	Pinhole	0	Imaging	2-Pixel Pinhole Grid	f
SubPixPinhole	Sub-Pix Pinhole	0	Imaging	Sub-Pixel Pinhole Gr	f
None	None	0	Imaging	Imaging (none)	f
Custom	Custom	0	MOS	Custom Mask	f
LongSlit1	Long Slit 1px	1	Long Slit	1-Pixel Long Slit	f
LongSlit2	Long Slit 2px	2	Long Slit	2-Pixel Long Slit	f
LongSlit3	Long Slit 3px	3	Long Slit	3-Pixel Long Slit	f
LongSlit4	Long Slit 4px	4	Long Slit	4-Pixel Long Slit	f
LongSlit6	Long Slit 6px	6	Long Slit	6-Pixel Long Slit	f
LongSlit8	Long Slit 8px	8	Long Slit	8-Pixel Long Slit	f
\.


--
-- Data for Name: e_f2_lyot_wheel; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_f2_lyot_wheel (id, short_name, plate_scale, pixel_scale, obsolete, long_name) FROM stdin;
F16	f/16	1.6100000000000001	0.179999999999999993	f	f/16 (Open)
F32High	f/32 High	0.805000000000000049	0.0899999999999999967	t	f/32 MCAO high background
F32Low	f/32 Low	0.805000000000000049	0.0899999999999999967	t	f/32 MCAO low background
F33Gems	f/33 GeMS	0.78400000000000003	0.0899999999999999967	t	f/33 (GeMS)
GemsUnder	GeMS Under	0.78400000000000003	0.0899999999999999967	f	f/33 (GeMS under-sized)
GemsOver	GeMS Over	0.78400000000000003	0.0899999999999999967	f	f/33 (GeMS over-sized)
HartmannA	Hartmann A (H1)	0	0	f	Hartmann A (H1)
HartmannB	Hartmann B (H2)	0	0	f	Hartmann B (H2)
\.


--
-- Data for Name: e_f2_read_mode; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_f2_read_mode (id, short_name, long_name, description, minimum_exposure_time, recommended_exposure_time, readout_time, read_count, read_noise) FROM stdin;
Bright	bright	Bright Object	Strong Source	1500	5000	8000	1	11.7
Medium	medium	Medium Object	Medium Source	6000	21000	14000	4	6.0
Faint	faint	Faint Object	Weak Source	12000	85000	20000	8	5.0
\.


--
-- Data for Name: e_f2_window_cover; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_f2_window_cover (id, short_name, long_name) FROM stdin;
Open	Open	Open
Close	Close	Close
\.


--
-- Data for Name: e_gcal_filter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gcal_filter (id, short_name, obsolete, long_name) FROM stdin;
None	none	f	none
Gmos	GMOS balance	f	GMOS balance
Hros	HROS balance	t	HROS balance
Nir	NIR balance	f	NIR balance
Nd10	ND1.0	f	ND1.0
Nd16	ND1.6	t	ND1.6
Nd20	ND2.0	f	ND2.0
Nd30	ND3.0	f	ND3.0
Nd40	ND4.0	f	ND4.0
Nd45	ND4-5	f	ND4-5
Nd50	ND5.0	t	ND5.0
\.


--
-- Data for Name: e_gcal_continuum; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gcal_continuum (id, short_name, long_name, obsolete) FROM stdin;
IrGreyBodyLow	IR grey body - low	IR grey body - low	f
IrGreyBodyHigh	IR grey body - high	IR grey body - high	f
QuartzHalogen	Quartz Halogen	Quartz Halogen	f
\.


--
-- Data for Name: e_gcal_arc; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gcal_arc (id, short_name, long_name, obsolete) FROM stdin;
ArArc	Ar arc	Ar arc	f
ThArArc	ThAr arc	ThAr arc	f
CuArArc	CuAr arc	CuAr arc	f
XeArc	Xe arc	Xe arc	f
\.


--
-- Data for Name: e_gcal_diffuser; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gcal_diffuser (id, short_name, obsolete, long_name) FROM stdin;
Ir	IR	f	IR
Visible	Visible	f	Visible
\.


--
-- Data for Name: e_gcal_shutter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gcal_shutter (id, short_name, obsolete, long_name) FROM stdin;
Open	Open	f	Open
Closed	Closed	f	Closed
\.


--
-- Data for Name: e_instrument; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_instrument (id, short_name, long_name, obsolete) FROM stdin;
Phoenix	Phoenix	Phoenix	f
Michelle	Michelle	Michelle	f
Gnirs	GNIRS	GNIRS	f
Niri	NIRI	NIRI	f
Trecs	TReCS	TReCS	f
Nici	NICI	NICI	f
Nifs	NIFS	NIFS	f
Gpi	GPI	GPI	f
Gsaoi	GSAOI	GSAOI	f
GmosS	GMOS-S	GMOS South	f
AcqCam	AcqCam	Acquisition Camera	f
GmosN	GMOS-N	GMOS North	f
Bhros	bHROS	bHROS	t
Visitor	Visitor Instrument	Visitor Instrument	f
Flamingos2	Flamingos2	Flamingos 2	f
\.


--
-- Data for Name: e_program_role; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_program_role (id, short_name, long_name) FROM stdin;
PI	PI	Principal Investigator
GEM	GEM	Gemini Contact
NGO	NGO	NGO Contact
\.


--
-- Data for Name: e_program_type; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_program_type (id, short_name, long_name, obsolete) FROM stdin;
CAL	CAL	Calibration	f
C	C	Classical	f
DS	DS	Demo Science	f
DD	DD	Director's Time	f
ENG	ENG	Engineering	f
FT	FT	Fast Turnaround	f
LP	LP	Large Program	f
Q	Q	Queue	f
SV	SV	System Verification	f
\.


--
-- Data for Name: e_site; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_site (id, long_name, mountain, longitude, latitude, altitude, timezone, short_name) FROM stdin;
GN	Gemini North	Mauna Kea	-155.469055	19.8238068	4213	Pacific/Honolulu	GN
GS	Gemini South	Cerro Pachon	-70.7366867	-30.2407494	2722	America/Santiago	GS
\.


--
-- Data for Name: e_template; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_template (id, short_name, long_name, obsolete) FROM stdin;
\.


--
-- Data for Name: gem_user; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY gem_user (id, first, last, md5, email, staff) FROM stdin;
root			                                	 	t
\.


--
-- Data for Name: gem_user_program; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY gem_user_program (user_id, program_id, program_role) FROM stdin;
\.


--
-- Data for Name: log; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY log ("timestamp", level, program, message, stacktrace, elapsed, user_id) FROM stdin;
\.


--
-- Data for Name: observation; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY observation (program_id, observation_index, title, observation_id, instrument) FROM stdin;
\.


--
-- Data for Name: program; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY program (program_id, semester_id, site, program_type, index, day, title) FROM stdin;
\.


--
-- Data for Name: semester; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY semester (semester_id, year, half) FROM stdin;
2006A	2006	A
2006B	2006	B
2007B	2007	B
2012B	2012	B
\.


--
-- Name: e_program_role_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY e_program_role
    ADD CONSTRAINT e_program_role_pkey PRIMARY KEY (id);


--
-- Name: e_program_role_short_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY e_program_role
    ADD CONSTRAINT e_program_role_short_name_key UNIQUE (short_name);


--
-- Name: e_program_type_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY e_program_type
    ADD CONSTRAINT e_program_type_pkey PRIMARY KEY (id);


--
-- Name: e_site_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY e_site
    ADD CONSTRAINT e_site_pkey PRIMARY KEY (id);


--
-- Name: gen_user_program_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY gem_user_program
    ADD CONSTRAINT gen_user_program_pkey PRIMARY KEY (user_id, program_id, program_role);


--
-- Name: observation_instrument_observation_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY observation
    ADD CONSTRAINT observation_instrument_observation_id_key UNIQUE (instrument, observation_id);


--
-- Name: program_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY program
    ADD CONSTRAINT program_pkey PRIMARY KEY (program_id);


--
-- Name: semester_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY semester
    ADD CONSTRAINT semester_pkey PRIMARY KEY (semester_id);


--
-- Name: user_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY gem_user
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: ix_observation_instrument; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_observation_instrument ON observation USING btree (instrument);


--
-- Name: ix_observation_program_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_observation_program_id ON observation USING btree (program_id);


--
-- Name: ix_program_id; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_program_id ON program USING btree (program_id);


--
-- Name: ix_step; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_step ON step USING btree (observation_id, location);


--
-- Name: ix_step_oid; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_step_oid ON step USING btree (observation_id);


--
-- Name: ix_timestamp; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX ix_timestamp ON log USING btree ("timestamp" DESC);


--
-- Name: FK_gen_user_program_1; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY gem_user_program
    ADD CONSTRAINT "FK_gen_user_program_1" FOREIGN KEY (user_id) REFERENCES gem_user(id);


--
-- Name: FK_gen_user_program_2; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY gem_user_program
    ADD CONSTRAINT "FK_gen_user_program_2" FOREIGN KEY (program_id) REFERENCES program(program_id);


--
-- Name: FK_gen_user_program_3; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY gem_user_program
    ADD CONSTRAINT "FK_gen_user_program_3" FOREIGN KEY (program_role) REFERENCES e_program_role(id);


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
-- Name: log_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY log
    ADD CONSTRAINT log_user_id_fkey FOREIGN KEY (user_id) REFERENCES gem_user(id);


--
-- Name: observation_instrument_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY observation
    ADD CONSTRAINT observation_instrument_fkey FOREIGN KEY (instrument) REFERENCES e_instrument(id);


--
-- Name: program_site_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY program
    ADD CONSTRAINT program_site_fkey FOREIGN KEY (site) REFERENCES e_site(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--
