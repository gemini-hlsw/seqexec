
CREATE TYPE evt_lifecycle AS ENUM (
    'Start',
    'End'
);

ALTER TYPE evt_lifecycle OWNER TO postgres;

CREATE TYPE evt_observe_stage AS ENUM (
    'Sequence',
    'Slew',
    'Visit'
);

ALTER TYPE evt_observe_stage OWNER TO postgres;

create TYPE evt_observe_ctrl AS ENUM (
    'Abort',
    'Continue',
    'Pause',
    'Stop'
);

ALTER TYPE evt_observe_ctrl OWNER TO postgres;


CREATE TABLE log_observe_stage
(
   id            SERIAL,
   "timestamp"   timestamp (5) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
   lifecycle     evt_lifecycle NOT NULL,
   observe_stage evt_observe_stage NOT NULL,
   sequence_id   text NOT NULL
);
 
ALTER TABLE log_observe_stage OWNER TO postgres;

ALTER TABLE ONLY log_observe_stage
    ADD CONSTRAINT log_observe_stage_pkey PRIMARY KEY (id);

CREATE INDEX ix_log_observe_stage_timestamp ON log_observe_stage USING btree ("timestamp" DESC);


CREATE TABLE log_observe_ctrl
(
   id            SERIAL,
   "timestamp"   timestamp (5) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
   observe_ctrl  evt_observe_ctrl NOT NULL,
   sequence_id   text NOT NULL,
   why           text
);
 
ALTER TABLE log_observe_ctrl OWNER TO postgres;

ALTER TABLE ONLY log_observe_ctrl
    ADD CONSTRAINT log_observe_ctrl_pkey PRIMARY KEY (id);

CREATE INDEX ix_log_observe_ctrl_timestamp ON log_observe_ctrl USING btree ("timestamp" DESC);


CREATE TABLE log_observe_int
(
   id            SERIAL,
   "timestamp"   timestamp (5) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
   lifecycle     evt_lifecycle NOT NULL,
   sequence_id   text NOT NULL,
   step          integer NOT NULL
);
 
ALTER TABLE log_observe_int OWNER TO postgres;

ALTER TABLE ONLY log_observe_int
    ADD CONSTRAINT log_observe_int_pkey PRIMARY KEY (id);

CREATE INDEX ix_log_observe_int_timestamp ON log_observe_int USING btree ("timestamp" DESC);


