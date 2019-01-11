---------------------------
-- DHS Keyword Names
-- This table list the DHS exceptions, of keywords names that don't follow
-- the keyword name restrictions
---------------------------

CREATE TABLE e_dhs_keyword_names (
    keyword identifier  PRIMARY KEY NOT NULL REFERENCES e_fits_keyword_names ON DELETE CASCADE,
    name VARCHAR(70)             NOT NULL CHECK (name ~ '[a-zA-Z0-9_-]*')
);

ALTER TABLE e_dhs_keyword_names OWNER TO postgres;

COPY e_dhs_keyword_names (keyword, name) FROM stdin;
INSTRUMENT	instrument
OBSID	obsid
TELESCOP	telescope
\.