
-------------------------------------------------------------------------------
-- Observation Updates
--
-- * Establish (program_id, observation_index, instrument) as primary key.
--   This (a) provides an index that will work for bulk queries on program_id,
--   and (b) serves as a foreign key referent from various other tables
--   guaranteeing that they use the same instrument as the observation.
--
-- * Keep observation_id column and constraint as before:
--     program_id + "-" + observation_index
--
-- * Keep a unique constraint on observation_id to prevent two different
--   instruments for the same observation and gain an index on observation_id.
--
-- * Drop the static_ic column, since we are dropping the static_config table
--   and replacing the static_id with the observation reference triple
--   (program_id, observation_index, instrument)
-------------------------------------------------------------------------------

DROP INDEX ix_observation_instrument;

DROP INDEX ix_observation_program_id;

ALTER TABLE observation
  DROP CONSTRAINT  observation_static_id_fkey,
  DROP COLUMN      static_id,
  DROP CONSTRAINT  observation_pkey CASCADE,
  DROP CONSTRAINT  observation_instrument_observation_id_key,
  ADD  PRIMARY KEY (program_id, observation_index, instrument),
  ADD  CONSTRAINT  unique_observation_id UNIQUE (observation_id);


-------------------------------------------------------------------------------
-- Static Configuration Updates
--
-- * Removes the static_config table altogether, flattening the hierarchy.
--   We are then left with individual instrument static configuration tables
--   like static_f2, static_gmos_north and their subtables like gmos_custom_roi.
--   These will refer directly to their corresponding observation.
--
-- * Uses the triple (program_id, observation_index, instrument) as a FK into
--   observation.  This gurantees that he instrument matches and facilitates
--   bulk load queries for the program as a whole.
-------------------------------------------------------------------------------

ALTER TABLE gmos_custom_roi
  DROP COLUMN      static_id,
  ADD  COLUMN      program_id        text     NOT NULL,
  ADD  COLUMN      observation_index id_index NOT NULL,
  ADD  CONSTRAINT  observation_ref FOREIGN KEY (program_id, observation_index, instrument) REFERENCES observation ON DELETE CASCADE;

ALTER TABLE gmos_nod_and_shuffle
  DROP COLUMN      static_id,
  ADD  COLUMN      program_id        text     NOT NULL,
  ADD  COLUMN      observation_index id_index NOT NULL,
  ADD  PRIMARY KEY (program_id, observation_index, instrument),
  ADD  CONSTRAINT  observation_ref FOREIGN KEY (program_id, observation_index, instrument) REFERENCES observation ON DELETE CASCADE;

ALTER TABLE static_f2
  DROP COLUMN      static_id,
  ADD  COLUMN      program_id        text     NOT NULL,
  ADD  COLUMN      observation_index id_index NOT NULL,
  ADD  PRIMARY KEY (program_id, observation_index, instrument),
  ADD  CONSTRAINT  observation_ref FOREIGN KEY (program_id, observation_index, instrument) REFERENCES observation ON DELETE CASCADE;

ALTER TABLE static_gmos_north
  DROP COLUMN      static_id,
  ADD  COLUMN      program_id        text     NOT NULL,
  ADD  COLUMN      observation_index id_index NOT NULL,
  ADD  PRIMARY KEY (program_id, observation_index, instrument),
  ADD  CONSTRAINT  observation_ref FOREIGN KEY (program_id, observation_index, instrument) REFERENCES observation ON DELETE CASCADE;

ALTER TABLE static_gmos_south
  DROP COLUMN      static_id,
  ADD  COLUMN      program_id        text     NOT NULL,
  ADD  COLUMN      observation_index id_index NOT NULL,
  ADD  PRIMARY KEY (program_id, observation_index, instrument),
  ADD  CONSTRAINT  observation_ref FOREIGN KEY (program_id, observation_index, instrument) REFERENCES observation ON DELETE CASCADE;

DROP TABLE static_config;


-------------------------------------------------------------------------------
-- Dynamic Configuration Updates
--
-- Here we just switch from observation_id to (program_id, observation_index).
-------------------------------------------------------------------------------

ALTER TABLE step
  ADD  COLUMN     program_id        text     NOT NULL,
  ADD  COLUMN     observation_index id_index NOT NULL,
  ADD  CONSTRAINT observation_ref FOREIGN KEY (program_id, observation_index, instrument) REFERENCES observation ON DELETE CASCADE,
  DROP COLUMN     observation_id,
  ADD  CONSTRAINT unique_location UNIQUE (program_id, observation_index, location);
