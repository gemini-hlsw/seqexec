-- 
-- Copies the gcal table to smartgcal.  gcal will be used with gcal steps,
-- smartgcal with lookups.
--

-- First create the new smartgcal table matching the existing gcal table.
CREATE TABLE smartgcal
       (LIKE gcal INCLUDING ALL);

-- For some reason the foreign key constraints aren't copied over, so do it
-- now.
ALTER TABLE smartgcal
        ADD CONSTRAINT gcal_continuum_fkey FOREIGN KEY (continuum) REFERENCES e_gcal_continuum ON DELETE CASCADE;

ALTER TABLE smartgcal
        ADD CONSTRAINT gcal_diffuser_fkey FOREIGN KEY (diffuser) REFERENCES e_gcal_diffuser ON DELETE CASCADE;

ALTER TABLE smartgcal
        ADD CONSTRAINT gcal_filter_fkey FOREIGN KEY (filter) REFERENCES e_gcal_filter ON DELETE CASCADE;

ALTER TABLE smartgcal
        ADD CONSTRAINT gcal_shutter_fkey FOREIGN KEY (shutter) REFERENCES e_gcal_shutter ON DELETE CASCADE;
	
-- Copy over the smartgcal data that we imported earlier.
INSERT INTO smartgcal
     SELECT * FROM gcal;

-- Rename the primary key to smart_id.
ALTER TABLE smartgcal
     RENAME gcal_id TO smart_id;

-- The smartgcal table doesn't associate gcal configs with steps, so drop
-- the step_id column.
ALTER TABLE smartgcal
       DROP step_id;

-- Add a smart_id column to smart_f2 and smart_gmos_north
ALTER TABLE smart_f2
        ADD COLUMN smart_id integer;

ALTER TABLE smart_gmos_north
        ADD COLUMN smart_id integer;

-- Copy the gcal_id over to smart_id in smart_f2, since we have data for it.
UPDATE smart_f2 
   SET smart_id = gcal_id;

-- Drop the gcal_id from smart_f2 and smart_gmos_north since it will not
-- be used.
ALTER TABLE smart_f2
       DROP gcal_id;

ALTER TABLE smart_gmos_north
       DROP gcal_id;

-- Make smart_id a foreign key into smartgcal.
ALTER TABLE smart_f2
      ALTER smart_id SET NOT NULL;

ALTER TABLE smart_gmos_north
      ALTER smart_id SET NOT NULL;

ALTER TABLE smart_f2
        ADD CONSTRAINT smart_f2_smart_id_fkey FOREIGN KEY (smart_id) REFERENCES smartgcal ON DELETE CASCADE;

ALTER TABLE smart_gmos_north
        ADD CONSTRAINT smart_gmos_north_smart_id_fkey FOREIGN KEY (smart_id) REFERENCES smartgcal ON DELETE CASCADE;

-- Remove all the smartgcal data from gcal, since we've copied it over.
TRUNCATE TABLE gcal CASCADE;

-- The step_id is no longer optional in the original gcal table.
ALTER TABLE gcal 
      ALTER step_id SET NOT NULL;

