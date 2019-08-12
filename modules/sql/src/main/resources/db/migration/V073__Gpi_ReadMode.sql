---------------------------
-- Gpi read mode
---------------------------

INSERT INTO e_giapi_status_apply(id, instrument_id, type, status_item, apply_item)
VALUES
	('IFSReadMode', 'Gpi', 'Int', 'gpi:currentReadMode', 'gpi:configIfs.readoutMode');

--
-- Name: e_gpi_read_mode; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE e_gpi_read_mode (
  id          identifier  PRIMARY KEY,
  short_name  text        NOT NULL,
  value       smallint    NOT NULL
);

ALTER TABLE e_gpi_read_mode OWNER TO postgres;

--
-- Data for Name: e_gpi_read_mode; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY e_gpi_read_mode(id, short_name, value) FROM stdin;
Single	Single	1
CDS	CDS	2
MCDS	MCDS	3
UTR	UTR	4
\.
