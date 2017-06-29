--
-- Adds an e_mos_preimaging type and migrates existing mos_preimaging columns
-- to use that type instead of boolean.
--


CREATE TABLE e_mos_preimaging (
    id          identifier            PRIMARY KEY,
    description character varying(22) NOT NULL,
    to_boolean  boolean               NOT NULL UNIQUE
);

ALTER TABLE e_mos_preimaging OWNER TO postgres;

COPY e_mos_preimaging (id, description, to_boolean) FROM stdin;
IsMosPreImaging	Is MOS Pre-imaging	true
IsNotMosPreImaging	Is Not MOS Pre-Imaging	false
\.

CREATE OR REPLACE FUNCTION toMosPreImaging (
  isPreImaging boolean
) RETURNS identifier AS $$
BEGIN
  IF isPreImaging THEN
    RETURN 'IsMosPreImaging';
  ELSE
    RETURN 'IsNotMosPreImaging';
  END IF;
END;
$$ LANGUAGE 'plpgsql';

ALTER TABLE static_f2
  ALTER COLUMN mos_preimaging TYPE identifier
  USING toMosPreImaging(mos_preimaging);

ALTER TABLE static_f2
  ADD CONSTRAINT FK_e_mos_preimaging_id FOREIGN KEY (mos_preimaging) REFERENCES e_mos_preimaging;

ALTER TABLE static_gmos_north
  ALTER COLUMN mos_preimaging TYPE identifier
  USING toMosPreImaging(mos_preimaging);

ALTER TABLE static_gmos_north
  ADD CONSTRAINT FK_e_mos_preimaging_id FOREIGN KEY (mos_preimaging) REFERENCES e_mos_preimaging;

ALTER TABLE static_gmos_south
  ALTER COLUMN mos_preimaging TYPE identifier
  USING toMosPreImaging(mos_preimaging);

ALTER TABLE static_gmos_south
  ADD CONSTRAINT FK_e_mos_preimaging_id FOREIGN KEY (mos_preimaging) REFERENCES e_mos_preimaging;
