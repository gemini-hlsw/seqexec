--
-- Adds program_id to user_target to facilitate bulk loading for program.
--

ALTER TABLE user_target
  ADD COLUMN program_id        text     NOT NULL REFERENCES program ON DELETE CASCADE,
  ADD COLUMN observation_index id_index NOT NULL,
  ADD CONSTRAINT observation_id_check CHECK (((observation_id)::text = (((program_id)::text || '-'::text) || observation_index)));

CREATE INDEX user_target_observation_index ON user_target (program_id, observation_index)

