--
-- Adds user targets to the model.
--


-- User Target Type
--
-- An enumeration of the various uses of target positions associated with an
-- observation apart from the main science targets.

CREATE TABLE e_user_target_type (
  id         identifier PRIMARY KEY,
  short_name TEXT       NOT NULL,
  long_name  TEXT       NOT NULL,
  obsolete   boolean    NOT NULL
);

ALTER TABLE e_user_target_type OWNER TO postgres;

COPY e_user_target_type (id, short_name, long_name, obsolete) FROM stdin;
BlindOffset	Blind Offset	Blind Offset	f
OffAxis	Off Axis	Off Axis	f
TuningStar	Tuning Star	Tuning Star	f
Other	Other	Other	f
\.


-- User Target Table
-- Associates a target, user target type, and an observation.

CREATE TABLE user_target (
  id               SERIAL     PRIMARY KEY,
  target_id        integer    NOT NULL REFERENCES target(id),
  user_target_type identifier NOT NULL REFERENCES e_user_target_type(id),
  observation_id   TEXT       NOT NULL REFERENCES observation ON DELETE CASCADE
);

ALTER TABLE user_target OWNER TO postgres;
