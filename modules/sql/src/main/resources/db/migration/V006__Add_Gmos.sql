--
-- Name: e_gmos_detector; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gmos_detector (
    id               identifier            PRIMARY KEY,
    short_name       character varying(10) NOT NULL,
    long_name        character varying(10) NOT NULL,
    north_pixel_size numeric(5,4)          NOT NULL,
    south_pixel_size numeric(5,4)          NOT NULL,
    shuffle_offset   smallint              NOT NULL,
    x_size           smallint              NOT NULL,
    y_size           smallint              NOT NULL,
    max_rois         smallint              NOT NULL
);

ALTER TABLE e_gmos_detector OWNER TO postgres;


--
-- Data for Name: e_gmos_detector; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gmos_detector (id, short_name, long_name, north_pixel_size, south_pixel_size, shuffle_offset, x_size, y_size, max_rois) FROM stdin;
E2V	E2V	E2V	0.0727	0.0730	1536	6144	4608	4
HAMAMATSU	Hamamatsu	Hamamatsu	0.0809	0.0809	1392	6144	4224	5
\.


--
-- Name: e_gmos_north_disperser; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gmos_north_disperser (
    id             identifier            PRIMARY KEY,
    short_name     character varying(10) NOT NULL,
    long_name      character varying(20) NOT NULL,
    ruling_density smallint              NOT NULL,
    obsolete       boolean               NOT NULL
);

ALTER TABLE e_gmos_north_disperser OWNER TO postgres;


--
-- Data for Name: e_gmos_north_disperser; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gmos_north_disperser (id, short_name, long_name, ruling_density, obsolete) FROM stdin;
B1200_G5301	B1200	B1200_G5301	1200	f
R831_G5302	R831	R831_G5302	831	f
B600_G5303	B600	B600_G5303	600	t
B600_G5307	B600	B600_G5307	600	f
R600_G5304	R600	R600_G5304	600	f
R400_G5305	R400	R400_G5305	400	f
R150_G5306	R150	R150_G5306	150	f
R150_G5308	R150	R150_G5308	150	f
\.


--
-- Name: e_gmos_north_filter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gmos_north_filter (
    id         identifier            PRIMARY KEY,
    short_name character varying(10) NOT NULL,
    long_name  character varying(30) NOT NULL,
    wavelength numeric(4,3)          NOT NULL,
    obsolete   boolean               NOT NULL
);

ALTER TABLE e_gmos_north_filter OWNER TO postgres;


--
-- Data for Name: e_gmos_north_filter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gmos_north_filter(id, short_name, long_name, wavelength, obsolete) FROM stdin;
GPrime	g	g_G0301	0.475	f
RPrime	r	r_G0303	0.630	f
IPrime	i	i_G0302	0.780	f
ZPrime	z	z_G0304	0.925	f
Z	Z	Z_G0322	0.876	f
Y	Y	Y_G0323	1.01	f
GG455	GG455	GG455_G0305	0.680	f
OG515	OG515	OG515_G0306	0.710	f
RG610	RG610	RG610_G0307	0.750	f
CaT	CaT	CaT_G0309	0.860	f
Ha	Ha	Ha_G0310	0.655	f
HaC	HaC	HaC_G0311	0.662	f
DS920	DS920	DS920_G0312	0.920	f
SII	SII	SII_G0317	0.672	f
OIII	OIII	OIII_G0318	0.499	f
OIIIC	OIIIC	OIIIC_G0319	0.514	f
HeII	HeII	HeII_G0320	0.468	f
HeIIC	HeIIC	HeIIC_G0321	0.478	f
HartmannA_RPrime	r+HartA	HartmannA_G0313 + r_G0303	0.630	f
HartmannB_RPrime	r+HartB	HartmannB_G0314 + r_G0303	0.630	f
GPrime_GG455	g+GG455	g_G0301 + GG455_G0305	0.506	f
GPrime_OG515	g+OG515	g_G0301 + OG515_G0306	0.536	f
RPrime_RG610	r+RG610	r_G0303 + RG610_G0307	0.657	f
IPrime_CaT	i+CaT	i_G0302 + CaT_G0309	0.815	f
ZPrime_CaT	z+CaT	z_G0304 + CaT_G0309	0.890	f
UPrime	u	u_G0308	0.350	t
\.

--
-- Name: e_gmos_south_disperser; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gmos_south_disperser (
    id             identifier            PRIMARY KEY,
    short_name     character varying(10) NOT NULL,
    long_name      character varying(20) NOT NULL,
    ruling_density smallint              NOT NULL,
    obsolete       boolean               NOT NULL
);

ALTER TABLE e_gmos_south_disperser OWNER TO postgres;


--
-- Data for Name: e_gmos_south_disperser; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gmos_south_disperser (id, short_name, long_name, ruling_density, obsolete) FROM stdin;
B1200_G5321	B1200	B1200_G5321	1200	f
R831_G5322	R831	R831_G5322	831	f
B600_G5323	B600	B600_G5323	600	f
R600_G5324	R600	R600_G5324	600	f
R400_G5325	R400	R400_G5325	400	f
R150_G5326	R150	R150_G5326	150	f
\.
