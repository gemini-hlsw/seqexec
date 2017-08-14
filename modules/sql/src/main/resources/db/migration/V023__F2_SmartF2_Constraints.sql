--
-- Fix constraints in smart_f2, since disperser and fpu are now optional.
--

ALTER TABLE smart_f2
      ALTER COLUMN disperser DROP NOT NULL,
      ALTER COLUMN fpu       DROP NOT NULL;
