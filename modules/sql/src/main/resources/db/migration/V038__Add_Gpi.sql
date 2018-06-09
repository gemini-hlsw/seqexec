--
-- Name: e_gpi_adc; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_adc (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  value       boolean     NOT NULL
);

ALTER TABLE e_gpi_adc OWNER TO postgres;

--
-- Data for Name: e_gpi_adc; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_adc(id, short_name, long_name, value) FROM stdin;
In	In	In	true
Out	Out	Out	false
\.
--
-- Name: e_gpi_filter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_filter (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  band        identifier  NOT NULL REFERENCES e_magnitude_band,
  obsolete    boolean     NOT NULL
);

ALTER TABLE e_gpi_filter OWNER TO postgres;

--
-- Data for Name: e_gpi_filter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_filter(id, short_name, long_name, band, obsolete) FROM stdin;
Y	Y	Y	Y	f
J	J	J	J	f
H	H	H	H	f
K1	K1	K1	K	f
K2	K2	K2	K	f
\.

--
-- Name: e_gpi_disperser; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_disperser (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL
);

ALTER TABLE e_gpi_disperser OWNER TO postgres;

--
-- Data for Name: e_gpi_disperser; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_disperser(id, short_name, long_name) FROM stdin;
PRISM	Prism	Prism
WOLLASTON	Wollaston	Prism
\.

--
-- Name: e_gpi_apodizer; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_apodizer (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  obsolete    boolean     NOT NULL
);

ALTER TABLE e_gpi_apodizer OWNER TO postgres;

--
-- Data for Name: e_gpi_apodizer; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_apodizer(id, short_name, long_name, obsolete) FROM stdin;
CLEAR	Clear	Clear	f
CLEARGP	CLEAR GP	CLEAR GP	f
APOD_Y	APOD_Y_56	APOD_Y_56	f
APOD_J	APOD_J_56	APOD_J_56	f
APOD_H	APOD_H_56	APOD_H_56	f
APOD_K1	APOD_K1_56	APOD_K1_56	f
APOD_K2	APOD_K2_56	APOD_K2_56	f
NRM	NRM	NRM	f
APOD_HL	APOD_HL	APOD_HL	f
APOD_STAR	APOD_star	APOD_star	t
ND3	ND3	ND3	f
\.

--
-- Name: e_gpi_lyot; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_lyot (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  obsolete    boolean     NOT NULL
);

ALTER TABLE e_gpi_lyot OWNER TO postgres;

--
-- Data for Name: e_gpi_lyot; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_lyot(id, short_name, long_name, obsolete) FROM stdin;
BLANK	Blank	Blank	f
LYOT_080m12_03	080m12_03	080m12_03	f
LYOT_080m12_04	080m12_04	080m12_04	f
LYOT_080_04	080_04	080_04	f
LYOT_080m12_06	080m12_06	080m12_06	f
LYOT_080m12_04_c	080m12_04_c	080m12_04_c	f
LYOT_080m12_06_03	080m12_06_03	080m12_06_03	f
LYOT_080m12_07	080m12_07	080m12_07	f
LYOT_080m12_10	080m12_10	080m12_10	f
OPEN	Open	Open	f
\.

--
-- Name: e_gpi_artificial_source_unit; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_artificial_source_unit (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  value       boolean     NOT NULL
);

ALTER TABLE e_gpi_artificial_source_unit OWNER TO postgres;

--
-- Data for Name: e_gpi_artificial_source_unit; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_artificial_source_unit(id, short_name, long_name, value) FROM stdin;
On	On	On	true
Off	Off	Off	false
\.

--
-- Name: e_gpi_entrance_shutter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_entrance_shutter (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  value       boolean     NOT NULL
);

ALTER TABLE e_gpi_entrance_shutter OWNER TO postgres;

--
-- Data for Name: e_gpi_entrance_shutter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_entrance_shutter(id, short_name, long_name, value) FROM stdin;
Open	Open	Open	true
Close	Close	Close	false
\.

--
-- Name: e_gpi_sience_arm_shutter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_sience_arm_shutter (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  value       boolean     NOT NULL
);

ALTER TABLE e_gpi_sience_arm_shutter OWNER TO postgres;

--
-- Data for Name: e_gpi_sience_arm_shutter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_sience_arm_shutter(id, short_name, long_name, value) FROM stdin;
Open	Open	Open	true
Close	Close	Close	false
\.

--
-- Name: e_gpi_cal_entrance_shutter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_cal_entrance_shutter (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  value       boolean     NOT NULL
);

ALTER TABLE e_gpi_cal_entrance_shutter OWNER TO postgres;

--
-- Data for Name: e_gpi_cal_entrance_shutter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_cal_entrance_shutter(id, short_name, long_name, value) FROM stdin;
Open	Open	Open	true
Close	Close	Close	false
\.

--
-- Name: e_gpi_reference_arm_shutter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_reference_arm_shutter (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  value       boolean     NOT NULL
);

ALTER TABLE e_gpi_reference_arm_shutter OWNER TO postgres;

--
-- Data for Name: e_gpi_reference_arm_shutter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_reference_arm_shutter(id, short_name, long_name, value) FROM stdin;
Open	Open	Open	true
Close	Close	Close	false
\.

--
-- Name: e_gpi_pupil_camera; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_pupil_camera (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  value       boolean     NOT NULL
);

ALTER TABLE e_gpi_pupil_camera OWNER TO postgres;

--
-- Data for Name: e_gpi_pupil_camera; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_pupil_camera(id, short_name, long_name, value) FROM stdin;
In	In	In	true
Out	Out	Out	false
\.

--
-- Name: e_gpi_fpm; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_fpm (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  long_name   text        NOT NULL,
  obsolete    boolean     NOT NULL
);

ALTER TABLE e_gpi_fpm OWNER TO postgres;

--
-- Data for Name: e_gpi_fpm; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_fpm(id, short_name, long_name, obsolete) FROM stdin;
OPEN	Open	Open	f
F50umPIN	50umPIN	50umPIN	f
WITH_DOT	WITH_DOT	WITH_DOT	f
SCIENCE	SCIENCE	SCIENCE	f
FPM_K1	FPM_K1	FPM_K1	f
FPM_H	FPM_H	FPM_H	f
FPM_J	FPM_J	FPM_J	f
FPM_Y	FPM_Y	FPM_Y	f
\.

--
-- Name: e_gpi_sampling_mode; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_sampling_mode (
  id          identifier PRIMARY KEY,
  short_name  text       NOT NULL,
  long_name   text       NOT NULL,
  obsolete    boolean    NOT NULL
);

ALTER TABLE e_gpi_sampling_mode OWNER TO postgres;

--
-- Data for Name: e_gpi_sampling_mode; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_sampling_mode(id, short_name, long_name, obsolete) FROM stdin;
FAST	Fast	Fast	f
SINGLE_CDS	Single CDS	Single CDS	f
MULTIPLE_CDS	Multiple CDS	Multiple CDS	f
UTR	UTR	UTR	f
\.

--
-- Name: e_gpi_cassegrain; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_cassegrain (
  id          identifier PRIMARY KEY,
  short_name  text       NOT NULL,
  long_name   text       NOT NULL,
  value       integer    NOT NULL
);

ALTER TABLE e_gpi_cassegrain OWNER TO postgres;

--
-- Data for Name: e_gpi_cassegrain; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_cassegrain(id, short_name, long_name, value) FROM stdin;
A0	0	0	0
A180	180	180	180
\.

--
-- Name: e_gpi_observing_mode; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_observing_mode (
  id                      identifier    PRIMARY KEY,
  short_name              text          NOT NULL,
  long_name               text          NOT NULL,
  filter                  identifier    REFERENCES e_gpi_filter,
  filter_iterable         boolean       NOT NULL,
  apodizer                identifier    REFERENCES e_gpi_apodizer,
  fpm                     identifier    REFERENCES e_gpi_fpm,
  lyot                    identifier    REFERENCES e_gpi_lyot,
  bright_limit_prism      numeric(4,3)  NULL,
  bright_limit_wollaston  numeric(4,3)  NULL,
  corresponding_h_mode    identifier    NOT NULL REFERENCES e_gpi_observing_mode,
  obsolete                boolean       NOT NULL
);

ALTER TABLE e_gpi_observing_mode OWNER TO postgres;

--
-- Data for Name: e_gpi_observing_mode; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_observing_mode(id, short_name, long_name, filter, filter_iterable, apodizer, fpm, lyot, bright_limit_prism, bright_limit_wollaston, corresponding_h_mode, obsolete) FROM stdin;
CORON_Y_BAND	Coronograph Y-band	Coronograph Y-band	Y	false	APOD_Y	FPM_Y	LYOT_080m12_03	0.5	3.0	CORON_H_BAND	f
CORON_J_BAND	Coronograph J-band	Coronograph J-band	J	false	APOD_J	FPM_J	LYOT_080m12_04	0.5	3.0	CORON_H_BAND	f
CORON_H_BAND	Coronograph H-band	Coronograph H-band	H	false	APOD_H	FPM_H	LYOT_080m12_04	0.5	3.0	CORON_H_BAND	f
CORON_K1_BAND	Coronograph K1-band	Coronograph K1-band	K1	false	APOD_K1	FPM_K1	LYOT_080m12_06_03	0.5	3.0	CORON_H_BAND	f
CORON_K2_BAND	Coronograph K2-band	Coronograph K2-band	K2	false	APOD_K2	FPM_K1	LYOT_080m12_07	0.5	3.0	CORON_H_BAND	f
H_STAR	H_STAR	H_STAR	H	false	APOD_STAR	FPM_H	LYOT_080m12_03	\N	\N	H_STAR	t
H_LIWA	H_LIWA	H_LIWA	H	false	APOD_HL	FPM_K1	LYOT_080m12_04	\N	\N	H_LIWA	f
DIRECT_Y_BAND	Y direct	Y direct	Y	true	CLEAR	SCIENCE	OPEN	5.5	7.5	DIRECT_H_BAND	f
DIRECT_J_BAND	J direct	J direct	J	true	CLEAR	SCIENCE	OPEN	5.5	7.5	DIRECT_H_BAND	f
DIRECT_H_BAND	H direct	H direct	H	true	CLEAR	SCIENCE	OPEN	5.5	7.5	DIRECT_H_BAND	f
DIRECT_K1_BAND	K1 direct	K1 direct	K1	true	CLEAR	SCIENCE	OPEN	5.5	7.5	DIRECT_H_BAND	f
DIRECT_K2_BAND	K2 direct	K2 direct	K2	true	CLEAR	SCIENCE	OPEN	5.5	7.5	DIRECT_H_BAND	f
NRM_Y	Non Redundant Mask Y	Non Redundant Mask Y	Y	true	NRM	SCIENCE	OPEN	\N	\N	NRM_H	f
NRM_J	Non Redundant Mask J	Non Redundant Mask J	J	true	NRM	SCIENCE	OPEN	\N	\N	NRM_H	f
NRM_H	Non Redundant Mask H	Non Redundant Mask H	H	true	NRM	SCIENCE	OPEN	\N	\N	NRM_H	f
NRM_K1	Non Redundant Mask K1	Non Redundant Mask K1	K1	true	NRM	SCIENCE	OPEN	\N	\N	NRM_H	f
NRM_K2	Non Redundant Mask K2	Non Redundant Mask K2	K2	true	NRM	SCIENCE	OPEN	\N	\N	NRM_H	f
DARK	Dark	Dark	H	false	APOD_H	FPM_H	BLANK	0.5	3.0	DARK	f
UNBLOCKED_Y	Y Unblocked	Y Unblocked	Y	false	APOD_Y	SCIENCE	LYOT_080m12_03	4.0	6.5	UNBLOCKED_Y	f
UNBLOCKED_J	J Unblocked	J Unblocked	J	false	APOD_J	SCIENCE	LYOT_080m12_04	4.0	6.5	UNBLOCKED_J	f
UNBLOCKED_H	H Unblocked	H Unblocked	H	false	APOD_H	SCIENCE	LYOT_080m12_04	4.0	6.5	UNBLOCKED_H	f
UNBLOCKED_K1	K1 Unblocked	K1 Unblocked	K1	false	APOD_K1	SCIENCE	LYOT_080m12_06_03	4.0	6.5	UNBLOCKED_K1	f
UNBLOCKED_K2	K2 Unblocked	K2 Unblocked	K2	false	APOD_K2	SCIENCE	LYOT_080m12_07	4.0	6.5	UNBLOCKED_K2	f
NONSTANDARD	Nonstandard	Nonstandard	\N	false	\N	\N	\N	\N	\N	NONSTANDARD	f
\.
