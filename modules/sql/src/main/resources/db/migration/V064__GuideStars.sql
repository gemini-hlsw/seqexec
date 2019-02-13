--
-- Initial Guide Star Support
--

-- An enumeration of the available guide probes / guide windows along with
-- their associated instrument, if any.
CREATE TABLE e_guider (
  id         text       PRIMARY KEY,
  instrument identifier REFERENCES e_instrument,
  short_name text       NOT NULL,
  long_name  text       NOT NULL,
  UNIQUE (id, instrument) -- FK referent
);

ALTER TABLE e_guider OWNER to postgres;

COPY e_guider (id, instrument, short_name, long_name) FROM stdin;
F2OI	Flamingos2	F2 OI	Flamingos2 OIWFS
GmosNOI	GmosN	GMOS-N OI	GMOS North OIWFS
GmosSOI	GmosS	GMOS-S OI	GMOS South OIWFS
P1GN	\N	P1 GN	PWFS1 North
P2GN	\N	P2 GN	PWFS2 North
P1GS	\N	P1 GS	PWFS1 South
P2GS	\N	P2 GS	PWFS2 South
\.


--
-- Guide groups are either automatically assigned and managed or else manual.
--

CREATE TYPE guide_group_type AS ENUM (
  'Automatic',
  'Manual'
);

ALTER TYPE guide_group_type OWNER TO postgres;


--
-- Guide groups are a collection of guide stars, each assigned to a distinct
-- guider.
--

-- TODO: GuideEnvironment: these are ordered so we'll need an group index :-(

CREATE TABLE guide_group (
  id                SERIAL           PRIMARY KEY,
  program_id        text             NOT NULL,
  observation_index id_index         NOT NULL,
  instrument        identifier       NOT NULL REFERENCES e_instrument,
  type              guide_group_type NOT NULL,
  selected          boolean          NOT NULL,
  FOREIGN KEY (program_id, observation_index, instrument) REFERENCES observation ON DELETE CASCADE
);

ALTER TABLE guide_group OWNER TO postgres;

-- Only one selected group per observation
CREATE UNIQUE INDEX selected_guide_group
  ON guide_group(program_id, observation_index)
  WHERE (selected);

-- Only one auto group per observation
CREATE UNIQUE INDEX auto_guide_group
  ON guide_group(program_id, observation_index)
  WHERE (type = 'Automatic');


--
-- A guide star ties a target to the guide group (and observation).
--

CREATE TABLE guide_star (
  id                SERIAL     PRIMARY KEY,
  group_id          integer    NOT NULL     REFERENCES guide_group(id) ON DELETE CASCADE,
  target_id         integer    NOT NULL     REFERENCES target(id),

  guider            identifier NOT NULL     REFERENCES e_guider(id),
  guider_instrument identifier              REFERENCES e_instrument, -- NOTE: This is nullable for PWFS
  FOREIGN KEY (guider, guider_instrument)   REFERENCES e_guider(id, instrument),

  program_id        text       NOT NULL,
  observation_index id_index   NOT NULL,
  instrument        identifier NOT NULL     REFERENCES e_instrument,
  FOREIGN KEY (program_id, observation_index, instrument) REFERENCES observation ON DELETE CASCADE,

  UNIQUE (group_id, guider), -- No group should have more than one guide star assigned to a particular guider
  CHECK ((guider_instrument IS NULL) OR (instrument = guider_instrument))
);

ALTER TABLE guide_star OWNER TO postgres;

