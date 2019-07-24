---------------------------
-- Giapi status items
---------------------------

CREATE TABLE e_giapi_status (
  id               identifier  NOT NULL,
  instrument_id    identifier  NOT NULL REFERENCES e_instrument(id),
  status_item      status_item NOT NULL,
  type             giapi_type  NOT NULL,
  PRIMARY KEY(id, instrument_id)
);

ALTER TABLE e_giapi_status OWNER TO postgres;

INSERT INTO e_giapi_status(id, instrument_id, type, status_item)
VALUES
	('Guiding', 'Gpi', 'Int', 'gpi:guiding'),
	('AlignAndCalibState', 'Gpi', 'Int', 'gpi:alignCalibMode');
