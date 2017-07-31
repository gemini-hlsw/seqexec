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
EOffsettingOn	Electronic Offsetting On	true
EOffsettingOff	Electronic Offsetting Off	false
\.

ALTER TABLE static_gmos_north
    ADD COLUMN a_offset_p  numeric(9,3) NOT NULL,
    ADD COLUMN a_offset_q  numeric(9,3) NOT NULL,
    ADD COLUMN b_offset_p  numeric(9,3) NOT NULL,
    ADD COLUMN b_offset_q  numeric(9,3) NOT NULL,
    ADD COLUMN e_offset    identifier   NOT NULL REFERENCES e_gmos_e_offsetting ON DELETE CASCADE,
    ADD COLUMN offset_rows integer      NOT NULL CHECK (offset_rows > 0),
    ADD COLUMN cycles      integer      NOT NULL CHECK (cycles      > 0);

ALTER TABLE static_gmos_south
    ADD COLUMN a_offset_p  numeric(9,3) NOT NULL,
    ADD COLUMN a_offset_q  numeric(9,3) NOT NULL,
    ADD COLUMN b_offset_p  numeric(9,3) NOT NULL,
    ADD COLUMN b_offset_q  numeric(9,3) NOT NULL,
    ADD COLUMN e_offset    identifier   NOT NULL REFERENCES e_gmos_e_offsetting ON DELETE CASCADE,
    ADD COLUMN offset_rows integer      NOT NULL CHECK (offset_rows > 0),
    ADD COLUMN cycles      integer      NOT NULL CHECK (cycles      > 0);