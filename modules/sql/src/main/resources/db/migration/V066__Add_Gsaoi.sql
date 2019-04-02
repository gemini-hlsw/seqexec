--
-- Name: e_gsaoi_read_mode; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gsaoi_read_mode (
    id                        identifier            PRIMARY KEY,
    short_name                character varying(8)  NOT NULL,
    long_name                 character varying(40) NOT NULL,
    ndr                       smallint              NOT NULL CHECK (ndr >= 0),
    read_noise                smallint              NOT NULL CHECK (read_noise >= 0),
    minimum_exposure_time     numeric(3,1)          NOT NULL CHECK (minimum_exposure_time >= 0),
    overhead                  smallint              NOT NULL CHECK (overhead >= 0)
);

ALTER TABLE e_gsaoi_read_mode OWNER TO postgres;

--
-- Data for Name: e_gsaoi_read_mode; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gsaoi_read_mode(id, short_name, long_name, ndr, read_noise, minimum_exposure_time, overhead) FROM stdin;
Bright	Bright	Bright Objects	2	28	5.3	10
Faint	Faint	Faint Objects / Broad-band Imaging	8	13	21.5	26
VeryFaint	V. Faint	Very Faint Objects / Narrow-band Imaging	16	10	42.5	48
\.

--
-- Name: e_gsaoi_filter; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gsaoi_filter (
    id                        identifier            PRIMARY KEY,
    short_name                character varying(9)  NOT NULL,
    long_name                 character varying(23) NOT NULL,
    wavelength                numeric(6,2)          NOT NULL CHECK (wavelength >= 0),
    read_mode_id              identifier            NOT NULL,
    exposure_time_50_50       numeric(7,2)          NOT NULL CHECK (exposure_time_50_50 >= 0),
    exposure_time_half_well   numeric(7,2)          NOT NULL CHECK (exposure_time_half_well >= 0),
    band_id                   identifier            NULL,
    FOREIGN KEY (read_mode_id) REFERENCES e_gsaoi_read_mode(id) ON DELETE CASCADE,
    FOREIGN KEY (band_id) REFERENCES e_magnitude_band(id) ON DELETE CASCADE
);

ALTER TABLE e_gsaoi_filter OWNER TO postgres;

--
-- Data for Name: e_gsaoi_filter; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gsaoi_filter(id, long_name, short_name, wavelength, read_mode_id, exposure_time_50_50, exposure_time_half_well, band_id) FROM stdin;
Z	Z (1.015 um)	Z	1.015	Faint	26.0	4619	J
HeI	HeI (1.083 um)	HeI	1.083	VeryFaint	72.6	21792	J
PaGamma	Pa(gamma) (1.094 um)	Pagma	1.094	VeryFaint	122.0	36585	J
JContinuum	J-continuum (1.207 um)	Jcont	1.207	VeryFaint	32.6	9793	J
J	J (1.250 um)	J	1.250	Faint	5.7	1004	J
H	H (1.635 um)	H	1.635	Bright	12.0	460	H
PaBeta	Pa(beta) (1.282 um)	Pabeta	1.282	Faint	21.8	3879	J
HContinuum	H-continuum (1.570 um)	Hcont	1.570	Faint	31.2	5545	H
CH4Short	CH4(short) (1.580 um)	CH4short	1.580	Faint	6.6	1174	H
FeII	[Fe II] (1.644 um)	FeII1644	1.644	Faint	24.9	4416	H
CH4Long	CH4(long) (1.690 um)	CH4long	1.690	Faint	6.8	1202	H
H20Ice	H20 ice (2.000 um)	H20ice	2.000	Faint	19.1	3395	K
HeI2p2s	HeI (2p2s) (2.058 um)	HeI2p2s	2.058	Faint	28.3	5032	K
KContinuum1	Ks-continuum (2.093 um)	Kcontshrt	2.093	Faint	7.8	6069	K
BrGamma	Br(gamma) (2.166 um)	Brgma	2.166	Faint	31.0	5496	K
KContinuum2	Kl-continuum (2.270 um)	Kcontlong	2.270	Faint	33.3	5911	K
KPrime	K(prime) (2.120 um)	Kprime	2.120	Bright	14.8	566	K
H2_1_0_S_1	H2 1-0 S(1) (2.122 um)	H2(1-0)	2.122	Faint	27.5	5400	K
KShort	K(short) (2.150 um)	Kshort	2.150	Bright	14.4	551	K
K	K (2.200 um)	K	2.200	Bright	12.3	470	K
H2_2_1_S_1	H2 2-1 S(1) (2.248 um)	H2(2-1)	2.248	Faint	32.6	5784	K
CO	CO (2.360 um)	CO2360	2.360	Faint	7.7	1370	K
Diffuser1	Diffuser1	Diffuser1	0.0	Bright	0.0	0	\N
Diffuser2	Diffuser2	Diffuser2	0.0	Bright	0.0	0	\N
Blocked	Blocked	Blocked	0.0	Bright	0.0	0	\N
\.

-- Name: e_gsaoi_utility_wheel; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gsaoi_utility_wheel (
    id                        identifier            PRIMARY KEY,
    short_name                character varying(5)  NOT NULL,
    long_name                 character varying(40) NOT NULL
);

ALTER TABLE e_gsaoi_utility_wheel OWNER TO postgres;

--
-- Data for Name: e_gsaoi_utility_wheel; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gsaoi_utility_wheel(id, long_name, short_name) FROM stdin;
ExtrafocalLens1	Extra-focal lens 1	xf 1
ExtrafocalLens2	Extra-focal lens 2	xf 2
PupilImager	Pupil Imager	pupil
Clear	Clear	clear
\.

-- Name: e_gsaoi_roi; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gsaoi_roi (
    id                        identifier            PRIMARY KEY,
    short_name                character varying(11)  NOT NULL,
    long_name                 character varying(40) NOT NULL
);

ALTER TABLE e_gsaoi_roi OWNER TO postgres;

--
-- Data for Name: e_gsaoi_roi; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gsaoi_roi(id, long_name, short_name) FROM stdin;
FullArray	Det2kx2k	Full Array
Array64	Det2kx2k	Array 64
Array128	Det2kx2k	Array 128
Array256	Det2kx2k	Array 256
Array512	Det2kx2k	Array 512
Array1k	Det2kx2k	Array 1K
Central64	Det2kx2k	Central 64
Central128	Det2kx2k	Central 128
Central256	Det2kx2k	Central 256
Central512	Det2kx2k	Central 512
Central1k	Det2kx2k	Central 1K
Central2k	Det2kx2k	Central 2K
\.

-- Name: e_gsaoi_odgw_pixels; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gsaoi_odgw_pixels (
    id                        identifier            PRIMARY KEY,
    pixels                    smallint NOT NULL CHECK(pixels > 0)
);

ALTER TABLE e_gsaoi_odgw_pixels OWNER TO postgres;

--
-- Data for Name: e_gsaoi_odgw_pixels; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gsaoi_odgw_pixels(id, pixels) FROM stdin;
Size4	4
Size6	6
Size8	8
Size16	16
Size32	32
Size64	64
\.
