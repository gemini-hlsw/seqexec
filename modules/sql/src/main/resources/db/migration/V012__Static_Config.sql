--
-- This migration adds support for separating the static vs. dynamic (step)
-- parts of the instrument configuration.  Each observation must have an
-- instrument and its static configuration.
--

-- At this point we have just one instrument, F2, and it has just one bit of
-- static configuration: the mos_preimaging flag.  In this migration we'll move
-- that flag from it's previous home in the dynamic (step) configuration to a
-- new static_f2 table.
ALTER TABLE step_f2
  DROP COLUMN mos_preimaging;

-- static_config houses a unique id for each static configuation.  Though the
-- id is sufficient for a primary key, we add the corresponding instrument as a
-- discriminant.  Over in observation, the static_id and instrument will be a
-- non-null foreign key into this table.  That ensures that the observation has
-- a static component and that it uses the same instrument as the observation
-- itself.
CREATE TABLE static_config (
    static_id  SERIAL,
    instrument identifier NOT NULL REFERENCES e_instrument,
    PRIMARY KEY (static_id, instrument)
);

ALTER TABLE static_config OWNER TO postgres;

-- Each instrument will have its own static configuration table.  This is the
-- static configuration table for F2, which has a payload of a single boolean
-- mos-preimaging flag.  We have the same (static_id, instrument) foreign key
-- into the static_config table and furthermore guarantee that the instrument
-- is always Flamingos2.
CREATE TABLE static_f2 (
    static_id       integer,
    instrument      identifier NOT NULL,
    mos_preimaging  boolean    NOT NULL,
    PRIMARY KEY (static_id, instrument),
    FOREIGN KEY (static_id, instrument) REFERENCES static_config ON DELETE CASCADE,
    CONSTRAINT is_f2 CHECK (instrument = 'Flamingos2')
);

ALTER TABLE static_f2 OWNER TO postgres;

-- Observation already has instrument, but we need to add the static_id and set
-- it up as a foreign key.
ALTER TABLE observation
  ADD static_id integer NOT NULL;

ALTER TABLE observation
  ADD FOREIGN KEY (static_id, instrument) REFERENCES static_config ON DELETE CASCADE;

ALTER TABLE observation
  ADD CONSTRAINT unique_static_per_obs UNIQUE (static_id);

-- Observation has a foreign key pointing to static_config, but if an
-- observation is deleted the corresponding static_config stays around.  This
-- trigger takes care of deleting the matching static_config when an observation
-- is deleted.  (Note the converse -- deleting a row in static_config -- will
-- cascade to observation and the static_f2 table.)
CREATE FUNCTION delete_static_config() RETURNS trigger AS $delete_static_config$
  BEGIN
    DELETE FROM static_config WHERE static_id = OLD.static_id;
    RETURN OLD;
  END;
$delete_static_config$ LANGUAGE plpgsql;

CREATE TRIGGER delete_static_config AFTER DELETE ON observation
  FOR EACH ROW EXECUTE PROCEDURE delete_static_config();