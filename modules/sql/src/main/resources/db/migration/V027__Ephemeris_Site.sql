
--
-- Adds a site column to the ephemeris table (and makes it part of its
-- primary key).
--

ALTER TABLE ephemeris
    DROP CONSTRAINT ephemeris_pkey,
    ADD COLUMN site identifier NOT NULL REFERENCES e_site ON DELETE CASCADE,
    ADD PRIMARY KEY (key_type, key, site, timestamp);

