--
-- Adds support for GMOS nod and shuffle configuration.
--

CREATE TABLE e_gmos_e_offsetting(
    id          identifier            PRIMARY KEY,
    description character varying(33) NOT NULL,
    to_boolean  boolean               NOT NULL UNIQUE
);

ALTER TABLE e_gmos_e_offsetting OWNER TO postgres;

COPY e_gmos_e_offsetting (id, description, to_boolean) FROM stdin;
On	Electronic Offsetting On	true
Off	Electronic Offsetting Off	false
\.

CREATE TABLE gmos_nod_and_shuffle(
    static_id   integer,
    instrument  identifier   NOT NULL,
    a_offset_p  numeric(9,3) NOT NULL,
    a_offset_q  numeric(9,3) NOT NULL,
    b_offset_p  numeric(9,3) NOT NULL,
    b_offset_q  numeric(9,3) NOT NULL,
    e_offset    identifier   NOT NULL REFERENCES e_gmos_e_offsetting ON DELETE CASCADE,
    offset_rows integer      NOT NULL CHECK (offset_rows > 0),
    cycles      integer      NOT NULL CHECK (cycles      > 0),
    PRIMARY KEY (static_id, instrument),
    FOREIGN KEY (static_id, instrument) REFERENCES static_config ON DELETE CASCADE,
    CONSTRAINT is_gmos CHECK ((instrument = 'GmosN') OR (instrument = 'GmosS'))
);