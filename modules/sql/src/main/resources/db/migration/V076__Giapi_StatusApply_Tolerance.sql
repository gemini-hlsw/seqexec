---------------------------
-- Giapi status apply
---------------------------

-- Add tolerance column
ALTER TABLE e_giapi_status_apply
    ADD COLUMN tolerance numeric(10, 4) NULL;

UPDATE e_giapi_status_apply SET tolerance = 1.0 where id = 'PolarizerAngle';
