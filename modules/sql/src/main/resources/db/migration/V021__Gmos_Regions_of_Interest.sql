--
-- Adds support for GMOS Regions of Interest
--

-- Drop e_gmos_builtin_roi and use e_gmo_roi instead (including custom roi)

DROP TABLE e_gmos_builtin_roi;

CREATE TABLE e_gmos_roi(
    id          identifier            PRIMARY KEY,
    short_name  character varying(33) NOT NULL,
    long_name   character varying(44) NOT NULL,
    obsolete    boolean               NOT NULL
);

ALTER TABLE e_gmos_roi OWNER TO postgres;

COPY e_gmos_roi (id, short_name, long_name, obsolete) FROM stdin;
FullFrame	full	Full Frame Readout	f
Ccd2	ccd2	CCD 2	f
CentralSpectrum	cspec	Central Spectrum	f
CentralStamp	stamp	Central Stamp	f
TopSpectrum	tspec	Top Spectrum	t
BottomSpectrum	bspec	Bottom Spectrum	t
Custom	custom	Custom ROI	f
\.


-- Create a table to hold the ROI(s) for an observation.

CREATE TABLE gmos_custom_roi(
    id         SERIAL     PRIMARY KEY,
    static_id  integer,
    instrument identifier NOT NULL,
    x_min      smallint   NOT NULL CHECK (x_min   > 0),
    y_min      smallint   NOT NULL CHECK (y_min   > 0),
    x_range    smallint   NOT NULL check (x_range > 0),
    y_range    smallint   NOT NULL check (y_range > 0),
    FOREIGN KEY (static_id, instrument) REFERENCES static_config ON DELETE CASCADE,
    CONSTRAINT is_gmos CHECK ((instrument = 'GmosN') OR (instrument = 'GmosS'))
);

ALTER TABLE gmos_custom_roi OWNER TO postgres;

ALTER TABLE step_gmos_common
  ADD COLUMN roi identifier NOT NULL REFERENCES e_gmos_roi ON DELETE CASCADE;
