ALTER TABLE dataset
  ALTER COLUMN dataset_label TYPE TEXT,
  ALTER COLUMN observation_id TYPE TEXT,
  ALTER COLUMN filename TYPE TEXT;

ALTER TABLE e_f2_disperser
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_f2_filter
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_f2_fpu
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_f2_lyot_wheel
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_f2_read_mode
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT,
  ALTER COLUMN description TYPE TEXT;

ALTER TABLE e_f2_window_cover
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gcal_arc
  ALTER COLUMN id TYPE identifier,
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gcal_continuum
  ALTER COLUMN id TYPE identifier,
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gcal_diffuser
  ALTER COLUMN id TYPE identifier,
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gcal_filter
  ALTER COLUMN id TYPE identifier,
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gcal_shutter
  ALTER COLUMN id TYPE identifier,
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_adc
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_amp_count
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_amp_gain
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_amp_read_mode
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_binning
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_custom_slit_width
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_detector
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_disperser_order
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_dtax
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_e_offsetting
  ALTER COLUMN description TYPE TEXT;

ALTER TABLE e_gmos_north_disperser
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_north_filter
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_north_fpu
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_north_stage_mode
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_roi
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_south_disperser
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_south_filter
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_south_fpu
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_gmos_south_stage_mode
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_instrument
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_magnitude_band
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_mos_preimaging
  ALTER COLUMN description TYPE TEXT;

ALTER TABLE e_program_role
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_program_type
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE e_site
  ALTER COLUMN long_name TYPE TEXT,
  ALTER COLUMN mountain TYPE TEXT,
  ALTER COLUMN timezone TYPE TEXT,
  ALTER COLUMN short_name TYPE TEXT;

ALTER TABLE e_template
  ALTER COLUMN short_name TYPE TEXT,
  ALTER COLUMN long_name TYPE TEXT;

ALTER TABLE ephemeris
  ALTER COLUMN ra_str TYPE TEXT,
  ALTER COLUMN dec_str TYPE TEXT;

ALTER TABLE gem_user
  ALTER COLUMN id TYPE TEXT,
  ALTER COLUMN first TYPE TEXT,
  ALTER COLUMN last TYPE TEXT,
  ALTER COLUMN email TYPE TEXT;

ALTER TABLE gem_user_program
  ALTER COLUMN user_id TYPE TEXT,
  ALTER COLUMN program_id TYPE TEXT;

ALTER TABLE log
  ALTER COLUMN program TYPE TEXT,
  ALTER COLUMN message TYPE TEXT,
  ALTER COLUMN user_id TYPE TEXT;


ALTER TABLE observation
  ALTER COLUMN program_id TYPE TEXT,
  ALTER COLUMN title TYPE TEXT,
  ALTER COLUMN observation_id TYPE TEXT;

ALTER TABLE program
  ALTER COLUMN program_id TYPE TEXT,
  ALTER COLUMN semester_id TYPE TEXT,
  ALTER COLUMN title TYPE TEXT;

ALTER TABLE semester
  ALTER COLUMN semester_id TYPE TEXT;

ALTER TABLE step
  ALTER COLUMN observation_id TYPE TEXT;

ALTER TABLE step_gmos_north
  ALTER COLUMN mdf_file_name TYPE TEXT;

ALTER TABLE step_gmos_south
  ALTER COLUMN mdf_file_name TYPE TEXT;

ALTER TABLE target
  ALTER COLUMN name TYPE TEXT,
  ALTER COLUMN ra_str TYPE TEXT,
  ALTER COLUMN dec_str TYPE TEXT,
  ALTER COLUMN epoch TYPE TEXT;
