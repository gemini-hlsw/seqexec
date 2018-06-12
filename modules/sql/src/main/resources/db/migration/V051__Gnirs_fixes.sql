UPDATE e_gnirs_fpu_other SET id = 'Pinhole3' WHERE id = 'Pinhole2';

-- Typo
UPDATE e_gnirs_decker SET short_name = 'Short camera slit' WHERE id = 'ShortCamLongSlit';

INSERT INTO e_gnirs_decker
         (id,                      short_name,       long_name,                     obsolete)
  VALUES ('LongCamCrossDispersed', 'Long camera XD', 'Long camera cross dispersed', false   );
