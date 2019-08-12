---------------------------
-- Gpi read mode
---------------------------

INSERT INTO e_giapi_status_apply(id, instrument_id, type, status_item, apply_item)
VALUES
	('IFSReadMode', 'Gpi', 'Int', 'gpi:currentReadMode', 'gpi:configIfs.readoutMode'),
	('IFSStartX', 'Gpi', 'Int', 'gpi:currentStartX', 'gpi:gpi:configIfs.startx'),
	('IFSStartY', 'Gpi', 'Int', 'gpi:currentStartY', 'gpi:gpi:configIfs.starty'),
	('IFSEndX', 'Gpi', 'Int', 'gpi:currentEndX', 'gpi:gpi:configIfs.endx'),
	('IFSEndY', 'Gpi', 'Int', 'gpi:currentEndY', 'gpi:gpi:configIfs.endy');

--
-- Name: e_gpi_read_mode; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_read_mode (
  id          identifier  PRIMARY KEY,
  long_name   text        NOT NULL,
  value       smallint    NOT NULL
);

ALTER TABLE e_gpi_read_mode OWNER TO postgres;

--
-- Data for Name: e_gpi_read_mode; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_read_mode(id, long_name, value) FROM stdin;
Single	Fast	1
CDS	Single CDS	2
MCDS	Multiple CDS	3
UTR	UTR	4
\.
