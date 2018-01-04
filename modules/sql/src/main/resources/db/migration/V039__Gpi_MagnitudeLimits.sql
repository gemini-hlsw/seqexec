--
-- Make the bright limit constraints in gpi_observing_mode have 2 digits
-- Add checks to forbid negati
--

ALTER TABLE e_gpi_observing_mode
      ALTER COLUMN bright_limit_prism     TYPE numeric(4,2),
      ALTER COLUMN bright_limit_wollaston TYPE numeric(4,2);
