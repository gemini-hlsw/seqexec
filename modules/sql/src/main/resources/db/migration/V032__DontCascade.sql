--
-- Remove "ON DELETE CASCADE" for foreign key references of enum tables.  When
-- an enum value is removed, we need to carefully migrate data.
--

ALTER TABLE ephemeris
  DROP CONSTRAINT ephemeris_key_type_fkey,
  ADD  CONSTRAINT ephemeris_key_type_fkey FOREIGN KEY (key_type) REFERENCES e_ephemeris_type(id),
  DROP CONSTRAINT ephemeris_site_fkey,
  ADD  CONSTRAINT ephemeris_site_fkey     FOREIGN KEY (site)     REFERENCES e_site(id);

ALTER TABLE ephemeris_meta
  DROP CONSTRAINT ephemeris_meta_key_type_fkey,
  ADD  CONSTRAINT ephemeris_meta_key_type_fkey FOREIGN KEY (key_type) REFERENCES e_ephemeris_type(id),
  DROP CONSTRAINT ephemeris_meta_site_fkey,
  ADD  CONSTRAINT ephemeris_meta_site_fkey     FOREIGN KEY (site)     REFERENCES e_site(id);

ALTER TABLE gcal
  DROP CONSTRAINT gcal_continuum_fkey,
  ADD  CONSTRAINT gcal_continuum_fkey FOREIGN KEY (continuum) REFERENCES e_gcal_continuum(id),
  DROP CONSTRAINT gcal_diffuser_fkey,
  ADD  CONSTRAINT gcal_diffuser_fkey  FOREIGN KEY (diffuser)  REFERENCES e_gcal_diffuser(id),
  DROP CONSTRAINT gcal_filter_fkey,
  ADD  CONSTRAINT gcal_filter_fkey    FOREIGN KEY (filter)    REFERENCES e_gcal_filter(id),
  DROP CONSTRAINT gcal_shutter_fkey,
  ADD  CONSTRAINT gcal_shutter_fkey   FOREIGN KEY (shutter)   REFERENCES e_gcal_shutter(id);

ALTER TABLE gmos_nod_and_shuffle
  DROP CONSTRAINT gmos_nod_and_shuffle_e_offset_fkey,
  ADD  CONSTRAINT gmos_nod_and_shuffle_e_offset_fkey FOREIGN KEY (e_offset) REFERENCES e_gmos_e_offsetting(id);

ALTER TABLE smart_f2
  DROP CONSTRAINT smart_f2_disperser_fkey,
  ADD  CONSTRAINT smart_f2_disperser_fkey FOREIGN KEY (disperser) REFERENCES e_f2_disperser(id),
  DROP CONSTRAINT smart_f2_filter_fkey,
  ADD  CONSTRAINT smart_f2_filter_fkey    FOREIGN KEY (filter)    REFERENCES e_f2_filter(id),
  DROP CONSTRAINT smart_f2_fpu_fkey,
  ADD  CONSTRAINT smart_f2_fpu_fkey       FOREIGN KEY (fpu)       REFERENCES e_f2_fpu(id);

ALTER TABLE smart_gmos_north
  DROP CONSTRAINT smart_gmos_north_amp_gain_fkey,
  ADD  CONSTRAINT smart_gmos_north_amp_gain_fkey  FOREIGN KEY (amp_gain)  REFERENCES e_gmos_amp_gain(id),
  DROP CONSTRAINT smart_gmos_north_disperser_fkey,
  ADD  CONSTRAINT smart_gmos_north_disperser_fkey FOREIGN KEY (disperser) REFERENCES e_gmos_north_disperser(id),
  DROP CONSTRAINT smart_gmos_north_filter_fkey,
  ADD  CONSTRAINT smart_gmos_north_filter_fkey    FOREIGN KEY (filter)    REFERENCES e_gmos_north_filter(id),
  DROP CONSTRAINT smart_gmos_north_fpu_fkey,
  ADD  CONSTRAINT smart_gmos_north_fpu_fkey       FOREIGN KEY (fpu)       REFERENCES e_gmos_north_fpu(id),
  DROP CONSTRAINT smart_gmos_north_x_binning_fkey,
  ADD  CONSTRAINT smart_gmos_north_x_binning_fkey FOREIGN KEY (x_binning) REFERENCES e_gmos_binning(id),
  DROP CONSTRAINT smart_gmos_north_y_binning_fkey,
  ADD  CONSTRAINT smart_gmos_north_y_binning_fkey FOREIGN KEY (y_binning) REFERENCES e_gmos_binning(id);

ALTER TABLE smart_gmos_south
  DROP CONSTRAINT smart_gmos_south_amp_gain_fkey,
  ADD  CONSTRAINT smart_gmos_south_amp_gain_fkey  FOREIGN KEY (amp_gain)  REFERENCES e_gmos_amp_gain(id),
  DROP CONSTRAINT smart_gmos_south_disperser_fkey,
  ADD  CONSTRAINT smart_gmos_south_disperser_fkey FOREIGN KEY (disperser) REFERENCES e_gmos_south_disperser(id),
  DROP CONSTRAINT smart_gmos_south_filter_fkey,
  ADD  CONSTRAINT smart_gmos_south_filter_fkey    FOREIGN KEY (filter)    REFERENCES e_gmos_south_filter(id),
  DROP CONSTRAINT smart_gmos_south_fpu_fkey,
  ADD  CONSTRAINT smart_gmos_south_fpu_fkey       FOREIGN KEY (fpu)       REFERENCES e_gmos_south_fpu(id),
  DROP CONSTRAINT smart_gmos_south_x_binning_fkey,
  ADD  CONSTRAINT smart_gmos_south_x_binning_fkey FOREIGN KEY (x_binning) REFERENCES e_gmos_binning(id),
  DROP CONSTRAINT smart_gmos_south_y_binning_fkey,
  ADD  CONSTRAINT smart_gmos_south_y_binning_fkey FOREIGN KEY (y_binning) REFERENCES e_gmos_binning(id);

ALTER TABLE step_f2
  DROP CONSTRAINT step_f2_disperser_fkey,
  ADD  CONSTRAINT step_f2_disperser_fkey    FOREIGN KEY (disperser)    REFERENCES e_f2_disperser(id),
  DROP CONSTRAINT step_f2_filter_fkey,
  ADD  CONSTRAINT step_f2_filter_fkey       FOREIGN KEY (filter)       REFERENCES e_f2_filter(id),
  DROP CONSTRAINT step_f2_fpu_fkey,
  ADD  CONSTRAINT step_f2_fpu_fkey          FOREIGN KEY (fpu)          REFERENCES e_f2_fpu(id),
  DROP CONSTRAINT step_f2_lyot_wheel_fkey,
  ADD  CONSTRAINT step_f2_lyot_wheel_fkey   FOREIGN KEY (lyot_wheel)   REFERENCES e_f2_lyot_wheel(id),
  DROP CONSTRAINT step_f2_read_mode_fkey,
  ADD  CONSTRAINT step_f2_read_mode_fkey    FOREIGN KEY (read_mode)    REFERENCES e_f2_read_mode(id),
  DROP CONSTRAINT step_f2_window_cover_fkey,
  ADD  CONSTRAINT step_f2_window_cover_fkey FOREIGN KEY (window_cover) REFERENCES e_f2_window_cover(id);

ALTER TABLE step_gcal
  DROP CONSTRAINT step_gcal_continuum_fkey,
  ADD  CONSTRAINT step_gcal_continuum_fkey FOREIGN KEY (continuum) REFERENCES e_gcal_continuum(id),
  DROP CONSTRAINT step_gcal_diffuser_fkey,
  ADD  CONSTRAINT step_gcal_diffuser_fkey  FOREIGN KEY (diffuser)  REFERENCES e_gcal_diffuser(id),
  DROP CONSTRAINT step_gcal_filter_fkey,
  ADD  CONSTRAINT step_gcal_filter_fkey    FOREIGN KEY (filter)    REFERENCES e_gcal_filter(id),
  DROP CONSTRAINT step_gcal_shutter_fkey,
  ADD  CONSTRAINT step_gcal_shutter_fkey   FOREIGN KEY (shutter)   REFERENCES e_gcal_shutter(id);

ALTER TABLE step_gmos_common
  DROP CONSTRAINT step_gmos_common_roi_fkey,
  ADD  CONSTRAINT step_gmos_common_roi_fkey FOREIGN KEY (roi) REFERENCES e_gmos_roi(id);


