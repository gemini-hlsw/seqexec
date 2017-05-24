--
-- Name: e_gmos_detector; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gmos_detector (
    id               identifier            PRIMARY KEY,
    short_name       character varying(20) NOT NULL,
    long_name        character varying(20) NOT NULL,
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



