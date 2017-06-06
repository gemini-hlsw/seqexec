
ALTER TABLE step_f2
  DROP COLUMN mos_preimaging;

CREATE TABLE static_config (
    static_id  SERIAL,
    instrument identifier NOT NULL    REFERENCES e_instrument,
    PRIMARY KEY (static_id, instrument)
);

ALTER TABLE static_config OWNER TO postgres;

CREATE TABLE static_f2 (
    static_id       integer,
    instrument      identifier NOT NULL,
    mos_preimaging  boolean    NOT NULL, 
    PRIMARY KEY (static_id, instrument),
    FOREIGN KEY (static_id, instrument) REFERENCES static_config ON DELETE CASCADE,
    CONSTRAINT is_f2 CHECK (instrument = 'Flamingos2')
);

ALTER TABLE static_f2 OWNER TO postgres;

ALTER TABLE observation
  ADD static_id integer NOT NULL;

ALTER TABLE observation
  ADD FOREIGN KEY (static_id, instrument) REFERENCES static_config ON DELETE CASCADE;

