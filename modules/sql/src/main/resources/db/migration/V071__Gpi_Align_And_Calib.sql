---------------------------
-- Gpi status apply for Align and calib
---------------------------

INSERT INTO e_giapi_status_apply(id, instrument_id, type, status_item, apply_item)
VALUES
	('AlignAndCalib', 'Gpi', 'Int', 'gpi:alignAndCalib.part1', 'gpi:alignAndCalib.part1');
