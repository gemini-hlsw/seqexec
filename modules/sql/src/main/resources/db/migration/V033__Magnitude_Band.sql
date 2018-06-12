--
-- Name: magnitude_system; Type: TYPE; Schema: public; Owner: postgres
--

-- Note: this may need to be turned into a table once we start taking into
-- account the units needed for ITC Watts and Ergs.
CREATE TYPE magnitude_system AS ENUM (
    'Vega',
    'AB',
    'Jy'
);

ALTER TYPE magnitude_system OWNER TO postgres;

COMMENT ON TYPE magnitude_system IS 'Integrated brightness units';

--
-- Name: e_magnitude_band; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_magnitude_band (
    id             identifier            PRIMARY KEY,
    short_name     character varying(2)  NOT NULL,
    long_name      character varying(14) NOT NULL,
    center         integer               NOT NULL,
    width          integer               NOT NULL,
    default_system magnitude_system      NOT NULL
);

ALTER TABLE e_magnitude_band OWNER TO postgres;

COMMENT ON COLUMN e_magnitude_band.center IS 'nm';
COMMENT ON COLUMN e_magnitude_band.width  IS 'nm';

--
-- Data for Name: e_magnitude_band; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_magnitude_band (id, short_name, long_name, center, width, default_system) FROM stdin;
SloanU	u	UV	356	46	AB
SloanG	g	Green	483	99	AB
SloanR	r	Red	626	96	AB
SloanI	i	Far red	767	106	AB
SloanZ	z	Near infrared	910	125	AB
U	U	Ultraviolet	360	75	Vega
B	B	Blue	440	90	Vega
V	V	Visual	550	85	Vega
Uc	UC	UCAC	610	63	Vega
R	R	Red	670	100	Vega
I	I	Infrared	870	100	Vega
Y	Y	Y	1020	120	Vega
J	J	J	1250	240	Vega
H	H	H	1650	300	Vega
K	K	K	2200	410	Vega
L	L	L	3760	700	Vega
M	M	M	4770	240	Vega
N	N	N	10470	5230	Vega
Q	Q	Q	20130	1650	Vega
Ap	AP	Apparent	550	85	Vega
\.
