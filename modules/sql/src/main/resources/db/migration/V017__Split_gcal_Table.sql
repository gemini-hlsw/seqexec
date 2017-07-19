--
-- Currently the gcal table is used to store both manual gcal step
-- configurations and smart gcal mappings.  In this migration we will update
-- the step_gcal table to include all the gcal config columns instead of having
-- a foreign key into gcal.  Then we will drop the step_id column from gcal. At
-- that point, step_gcal will directly house gcal configurations for manual
-- gcal steps while the gcal table only holds smart gcal values.
--

ALTER TABLE step_gcal
       DROP COLUMN     gcal_id,
        ADD COLUMN     continuum     identifier                         REFERENCES e_gcal_continuum ON DELETE CASCADE,
        ADD COLUMN     ar_arc        boolean    NOT NULL DEFAULT FALSE,
        ADD COLUMN     cuar_arc      boolean    NOT NULL DEFAULT FALSE,
        ADD COLUMN     thar_arc      boolean    NOT NULL DEFAULT FALSE,
        ADD COLUMN     xe_arc        boolean    NOT NULL DEFAULT FALSE,
        ADD COLUMN     filter        identifier NOT NULL                REFERENCES e_gcal_filter    ON DELETE CASCADE,
        ADD COLUMN     diffuser      identifier NOT NULL                REFERENCES e_gcal_diffuser  ON DELETE CASCADE,
        ADD COLUMN     shutter       identifier NOT NULL                REFERENCES e_gcal_shutter   ON DELETE CASCADE,
        ADD COLUMN     exposure_time bigint     NOT NULL,
        ADD COLUMN     coadds        coadds     NOT NULL,
        ADD CONSTRAINT check_lamp CHECK ((continuum IS NULL) = (ar_arc OR cuar_arc OR thar_arc OR xe_arc));

ALTER TABLE gcal
       DROP step_id;