
-- Alter the data types for times
ALTER TABLE ONLY e_f2_read_mode
    ALTER COLUMN minimum_exposure_time SET DATA TYPE numeric(3, 1) using minimum_exposure_time / 1000.0,
		ALTER COLUMN recommended_exposure_time SET DATA TYPE numeric(3, 1) using recommended_exposure_time / 1000.0,
    ALTER COLUMN readout_time SET DATA TYPE numeric(3, 1) using readout_time / 1000.0,
    ADD CHECK(minimum_exposure_time >= 0),
    ADD CHECK(recommended_exposure_time >= 0),
    ADD CHECK(readout_time >= 0);
