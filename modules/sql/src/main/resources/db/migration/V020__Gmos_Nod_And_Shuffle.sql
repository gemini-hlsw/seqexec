--
-- Adds support for GMOS nod and shuffle configuration.
--

CREATE TABLE e_gmos_e_offset(
    id          identifier            PRIMARY KEY,
    description character varying(33) NOT NULL,
    to_boolean  boolean               NOT NULL UNIQUE
);

ALTER TABLE e_gmos_e_offset OWNER TO postgres;

COPY e_gmos_e_offset (id, description, to_boolean) FROM stdin;
UseEOffseting	Use Electronic Offsetting	true
DoNotUseEOffsetting	Do Not Use Electronic Offsetting	false
\.

ALTER TABLE static_gmos_north
    ADD COLUMN a_offset_p  numeric(9,3) NOT NULL,
    ADD COLUMN a_offset_q  numeric(9,3) NOT NULL,
    ADD COLUMN b_offset_p  numeric(9,3) NOT NULL,
    ADD COLUMN b_offset_q  numeric(9,3) NOT NULL,
    ADD COLUMN e_offset    identifier   NOT NULL REFERENCES e_gmos_e_offset ON DELETE CASCADE,
    ADD COLUMN offset_rows integer      NOT NULL CHECK (offset_rows > 0),
    ADD COLUMN cycles      integer      NOT NULL CHECK (cycles      > 0);