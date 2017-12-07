--
-- Replace double and real by numeric(m,n)
--

ALTER TABLE e_site
      ALTER COLUMN longitude TYPE numeric(10,7),
      ALTER COLUMN latitude  TYPE numeric(10,7);

-- Some precision is lost during conversion
UPDATE e_site SET longitude = -155.469055, latitude =  19.8238068 WHERE id = 'GN';
UPDATE e_site SET longitude = -70.7366867, latitude = -30.2407494 WHERE id = 'GS';

ALTER TABLE e_f2_lyot_wheel
      ALTER COLUMN plate_scale TYPE numeric(4,3),
      ALTER COLUMN pixel_scale TYPE numeric(3,2);

ALTER TABLE e_f2_read_mode ALTER COLUMN read_noise TYPE numeric(3,1);
