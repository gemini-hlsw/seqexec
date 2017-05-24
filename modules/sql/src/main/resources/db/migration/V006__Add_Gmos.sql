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
-- Name: e_gmos_south_disperser; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gmos_south_disperser (
    id             identifier            PRIMARY KEY,
    short_name     character varying(10) NOT NULL,
    long_name      character varying(20) NOT NULL,
    ruling_density smallint              NOT NULL
);

ALTER TABLE e_gmos_south_disperser OWNER TO postgres;

--
-- Data for Name: e_gmos_south_disperser; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gmos_south_disperser (id, short_name, long_name, ruling_density) FROM stdin;
MIRROR	mirror	Mirror	0
B1200_G5321	B1200	B1200_G5321	1200
R831_G5322	R831	R831_G5322	831
B600_G5323	B600	B600_G5323	600
R600_G5324	R600	R600_G5324	600
R400_G5325	R400	R400_G5325	400
R150_G5326	R150	R150_G5326	150
\.
