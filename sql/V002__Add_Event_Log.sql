
CREATE TYPE evt_type AS ENUM (
    'StartSequence',
    'EndSequence',
    'StartSlew',
    'EndSlew',
    'StartVisit',
    'EndVisit',
    'StartIntegration',
    'EndIntegration',
    'Abort',
    'Continue',
    'Pause',
    'Stop'
);

CREATE TABLE log_observe_event
(
   id            SERIAL,
   "timestamp"   timestamp (5) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
   event         evt_type NOT NULL,
   sequence_id   text NOT NULL,
   step          integer
);

ALTER TABLE log_observe_event OWNER TO postgres;

ALTER TABLE ONLY log_observe_event
    ADD CONSTRAINT log_observe_event_pkey PRIMARY KEY (id);

CREATE INDEX ix_log_observe_event_timestamp ON log_observe_event USING btree ("timestamp" DESC);