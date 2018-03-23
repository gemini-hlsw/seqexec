DROP TYPE acquisition_mirror;

--
-- Name: e_gnirs_acquisition_mirror; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_acquisition_mirror (
    id         identifier PRIMARY KEY,
    short_name TEXT       NOT NULL,
    long_name  TEXT       NOT NULL
);

ALTER TABLE e_gnirs_acquisition_mirror OWNER TO postgres;

--
-- Data for Name: e_gnirs_acquisition_mirror; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_acquisition_mirror(id, short_name, long_name) FROM stdin;
In	In	In
Out	Out	Out
\.


ALTER TABLE step_gnirs
    ADD COLUMN acquisition_mirror identifier NOT NULL REFERENCES e_gnirs_acquisition_mirror;

--
-- Name: e_gnirs_acquisition_mirror; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gnirs_pixel_scale (
    id         identifier   PRIMARY KEY,
    short_name TEXT         NOT NULL,
    long_name  TEXT         NOT NULL,
    value      numeric(3,2) NOT NULL
);

ALTER TABLE e_gnirs_pixel_scale OWNER TO postgres;

--
-- Data for Name: e_gnirs_pixel_scale; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gnirs_pixel_scale(id, short_name, long_name, value) FROM stdin;
PixelScale_0_05	0.05 as/pix	Pixel scale for short cameras	0.05
PixelScale_0_15	0.15 as/pix	Pixel scale for long cameras	0.15
\.

COMMENT ON COLUMN e_gnirs_pixel_scale.value IS 'arcsec/pixels';

ALTER TABLE e_gnirs_camera
    DROP COLUMN pixel_scale,
    ADD COLUMN pixel_scale identifier REFERENCES e_gnirs_pixel_scale;

UPDATE e_gnirs_camera
    SET pixel_scale = 'PixelScale_0_05' WHERE id = 'LongBlue' OR id = 'LongRed';

UPDATE e_gnirs_camera
    SET pixel_scale = 'PixelScale_0_15' WHERE id = 'ShortBlue' OR id = 'ShortRed';

ALTER TABLE e_gnirs_camera ALTER COLUMN pixel_scale SET NOT NULL;
