--
-- Name: e_gnirs_cross_dispersed; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_prism (
    id              identifier   PRIMARY KEY,
    short_name      TEXT         NOT NULL,
    long_name       TEXT         NOT NULL
);

ALTER TABLE e_gnirs_prism OWNER TO postgres;

--
-- Data for Name: e_gnirs_prism; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_prism (id, short_name, long_name) FROM stdin;
Mirror	Mirror	Mirror
Sxd	Short XD	Short cross dispersion
Lxd	Long XD	Long cross dispersion
\.


--
-- Name: e_gnirs_fpu_slit; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_fpu_slit(
    id             identifier   PRIMARY KEY,
    short_name     TEXT         NOT NULL,
    long_name      TEXT         NOT NULL,
    slit_width     numeric(4,3) NOT NULL,
    obsolete       boolean      NOT NULL
);

ALTER TABLE e_gnirs_fpu_slit OWNER TO postgres;

--
-- Data for Name: e_gnirs_fpu_slit; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_fpu_slit (id, short_name, long_name, slit_width, obsolete) FROM stdin;
LongSlit1	0.10 arcsec	0.10 arcsec	0.10	f
LongSlit2	0.15 arcsec	0.15 arcsec	0.15	f
LongSlit3	0.20 arcsec	0.20 arcsec	0.20	f
LongSlit4	0.30 arcsec	0.30 arcsec	0.30	f
LongSlit5	0.45 arcsec	0.45 arcsec	0.45	f
LongSlit6	0.675 arcsec	0.675 arcsec	0.675	f
LongSlit7	1.0 arcsec	1.0 arcsec	1.0	f
LongSlit8	3.0 arcsec	3.0 arcsec	3.0	t
\.

COMMENT ON COLUMN e_gnirs_fpu_slit.slit_width IS 'arcsec';


--
-- Name: e_gnirs_fpu_other; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_fpu_other(
    id             identifier PRIMARY KEY,
    short_name     TEXT       NOT NULL,
    long_name      TEXT       NOT NULL,
    obsolete       boolean    NOT NULL
);

ALTER TABLE e_gnirs_fpu_other OWNER TO postgres;

--
-- Data for Name: e_gnirs_fpu_other; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_fpu_other (id, short_name, long_name, obsolete) FROM stdin;
Ifu	IFU	Integral Field Unit	t
Acquisition	Acquisition	Acquisition	f
PupilViewer	Pupil	Pupil viewer	f
Pinhole1	Small pin	pinhole 0.1	f
Pinhole2	Large pin	pinhole 0.3	f
\.


--
-- Name: e_gnirs_disperser; Type: TABLE DATA; Schema: public; Owner: postgres
--
CREATE TABLE e_gnirs_disperser (
    id             identifier PRIMARY KEY,
    short_name     TEXT       NOT NULL,
    long_name      TEXT       NOT NULL,
    ruling_density smallint   NOT NULL
);

ALTER TABLE e_gnirs_disperser OWNER TO postgres;

--
-- Data for Name: e_gnirs_disperser; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_disperser (id, short_name, long_name, ruling_density) FROM stdin;
D10	10	10 l/mm grating	10
D32	32	32 l/mm grating	32
D111	111	111 l/mm grating	111
\.

COMMENT ON COLUMN e_gnirs_disperser.ruling_density IS 'lines/mm';


--
-- Name: e_gnirs_disperser_order; Type: TABLE DATA; Schema: public; Owner: postgres
--
CREATE TABLE e_gnirs_disperser_order (
    id                 identifier   PRIMARY KEY,
    short_name         TEXT         NOT NULL,
    long_name          TEXT         NOT NULL,
    count              smallint     NOT NULL,
    default_wavelength numeric(4,3) NOT NULL,
    min_wavelength     numeric(3,2) NOT NULL,
    max_wavelength     numeric(3,2) NOT NULL,
    delta_wavelength   numeric(7,6) NOT NULL,
    band               identifier   REFERENCES e_magnitude_band,
    cross_dispersed    boolean      NOT NULL
);

ALTER TABLE e_gnirs_disperser_order OWNER TO postgres;

--
-- Data for Name: e_gnirs_disperser_order; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_disperser_order (id, short_name, long_name, count, default_wavelength, min_wavelength, max_wavelength, delta_wavelength, band, cross_dispersed) FROM stdin;
One	1	One	1	4.850	4.30	6.00	0.000000	M	f
Two	2	Two	2	3.400	2.70	4.30	0.000000	L	f
Three	3	Three	3	2.220	1.86	2.70	0.000647	K	t
FourXD	4XD	FourXD	4	1.650	1.42	1.86	0.000482	H	t
Four	4	Four	4	1.630	1.42	1.86	0.000485	H	t
Five	5	Five	5	1.250	1.17	1.42	0.000388	J	t
Six	6	Six	6	1.100	1.03	1.17	0.000323	\N	t
Seven	7	Seven	7	0.951	0.88	1.03	0.000276	\N	t
Eight	8	Eight	8	0.832	0.78	0.88	0.000241	\N	t
\.

COMMENT ON COLUMN e_gnirs_disperser_order.count              IS
  'Corresponding numeric value.  E.g., 0, 1, 2';
COMMENT ON COLUMN e_gnirs_disperser_order.default_wavelength IS
  'µm';
COMMENT ON COLUMN e_gnirs_disperser_order.min_wavelength     IS
  'µm';
COMMENT ON COLUMN e_gnirs_disperser_order.max_wavelength     IS
  'µm';
COMMENT ON COLUMN e_gnirs_disperser_order.delta_wavelength   IS
  'µm';
COMMENT ON COLUMN e_gnirs_disperser_order.cross_dispersed    IS
  'Availability in cross disperesed mode';


--
-- Name: e_gnirs_read_mode; Type: TABLE DATA; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_read_mode (
    id                    identifier   PRIMARY KEY,
    short_name            TEXT         NOT NULL,
    long_name             TEXT         NOT NULL,
    count                 smallint     NOT NULL UNIQUE,
    minimum_exposure_time milliseconds NOT NULL,
    read_noise            smallint     NOT NULL,
    read_noise_low        smallint     NOT NULL
);

ALTER TABLE e_gnirs_read_mode OWNER TO postgres;

--
-- Data for Name: e_gnirs_read_mode; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_read_mode (id, short_name, long_name, count, minimum_exposure_time, read_noise, read_noise_low) FROM stdin;
VeryBright	Very bright	Very Bright Acquisition or High Background	1	200	155	1
Bright	Bright	Bright objects	2	600	30	1
Faint	Faint	Faint objects	9000	3	10	16
VeryFaint	Very faint	Very faint objects	4	18000	7	32
\.

COMMENT ON COLUMN e_gnirs_disperser_order.count           IS
  'Numeric value for ordering.';
COMMENT ON COLUMN e_gnirs_read_mode.minimum_exposure_time IS
  'ms';
COMMENT ON COLUMN e_gnirs_read_mode.read_noise            IS
  'e-';
COMMENT ON COLUMN e_gnirs_read_mode.read_noise_low        IS
  'e-';


--
-- Name: e_gmos_north_filter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_filter (
    id         identifier   PRIMARY KEY,
    short_name TEXT         NOT NULL,
    long_name  TEXT         NOT NULL,
    wavelength numeric(3,2)
);

ALTER TABLE e_gnirs_filter OWNER TO postgres;

--
-- Data for Name: e_gnirs_filter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_filter(id, short_name, long_name, wavelength) FROM stdin;
CrossDispersed	XD	Cross dispersed	\N
Order6	X	Order 6 (X)	1.10
Order5	J	Order 5 (J)	1.25
Order4	H	Order 4 (H: 1.65µm)	1.65
Order3	K	Order 3 (K)	2.20
Order2	L	Order 2 (L)	3.50
Order1	M	Order 1 (M)	4.80
H2	H2	H2: 2.12µm	2.12
HNd100x	H+ND100X	H + ND100X	1.65
H2Nd100x	H2+ND100X	H2 + ND100X	2.12
PAH	PAH	PAH: 3.3µm	3.30
Y	Y	Y: 1.03µm	1.03
J	J	J: 1.25µm	1.25
K	K	K: 2.20µm	2.20
\.

COMMENT ON COLUMN e_gnirs_filter.wavelength IS 'µm';


--
-- Name: e_gnirs_camera; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_camera (
    id          identifier   PRIMARY KEY,
    short_name  TEXT         NOT NULL,
    long_name   TEXT         NOT NULL,
    pixel_scale numeric(3,2) NOT NULL
);

ALTER TABLE e_gnirs_camera OWNER TO postgres;

--
-- Data for Name: e_gnirs_camera; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_camera(id, short_name, long_name, pixel_scale) FROM stdin;
ShortBlue	Short blue	Short blue camera	0.15
LongBlue	Long blue	Long blue camera	0.05
ShortRed	Short red	Short red camera	0.15
LongRed	Long red	Long red camera	0.05
\.

COMMENT ON COLUMN e_gnirs_camera.pixel_scale IS 'arcsec/pixels';


--
-- Name: e_gnirs_decker; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_decker (
    id         identifier PRIMARY KEY,
    short_name TEXT       NOT NULL,
    long_name  TEXT       NOT NULL,
    obsolete   boolean    NOT NULL
);

ALTER TABLE e_gnirs_decker OWNER TO postgres;

--
-- Data for Name: e_gnirs_decker; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_decker(id, short_name, long_name, obsolete) FROM stdin;
Acquisition	Acquisition	Acquisition	f
PupilViewer	Pupil	Pupil viewer	f
ShortCamLongSlit	Shot camera slit	Short camera long slit	f
ShortCamCrossDispersed	Short camera XD	Short camera cross dispersed	f
Ifu	IFU	Integral field unit	t
LongCamLongSlit	Long camera slit	Long camera long slit	f
Wollaston	Wollaston	Wollaston	t
\.


--
-- Name: acquisition_mirror; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE acquisition_mirror AS ENUM (
    'In',
    'Out'
);

ALTER TYPE acquisition_mirror OWNER TO postgres;


--
-- Name: e_gnirs_well_depth; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_well_depth (
    id         identifier PRIMARY KEY,
    short_name TEXT       NOT NULL,
    long_name  TEXT       NOT NULL,
    bias_level smallint   NOT NULL
);

ALTER TABLE e_gnirs_well_depth OWNER TO postgres;

--
-- Data for Name: e_gnirs_well_depth; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_well_depth(id, short_name, long_name, bias_level) FROM stdin;
Shallow	Shallow	Shallow	300
Deep	Deep	Deep	600
\.

COMMENT ON COLUMN e_gnirs_well_depth.bias_level IS 'mV';


--
-- Name: e_gnirs_wavelength_suggestion; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_wavelength_suggestion (
    id         identifier PRIMARY KEY,
    short_name TEXT       NOT NULL,
    long_name  TEXT       NOT NULL,
    wavelength numeric(3,2)   NOT NULL
);

ALTER TABLE e_gnirs_wavelength_suggestion OWNER TO postgres;

--
-- Data for Name: e_gnirs_wavelength_suggestion; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_wavelength_suggestion(id, short_name, long_name, wavelength) FROM stdin;
One	4.85	4.85	4.85
Two	3.40	3.40	3.40
Three	2.22	2.22	2.22
Four	1.65	1.65	1.65
Five	1.63	1.63	1.63
Six	1.25	1.25	1.25
Seven	1.10	1.10	1.10
\.

COMMENT ON COLUMN e_gnirs_wavelength_suggestion.short_name IS 'µm';
COMMENT ON COLUMN e_gnirs_wavelength_suggestion.long_name  IS 'µm';
COMMENT ON COLUMN e_gnirs_wavelength_suggestion.wavelength IS 'µm';
