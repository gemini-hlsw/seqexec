--
-- Adds comments to various tables, columns, types.
--

COMMENT ON COLUMN e_f2_lyot_wheel.pixel_scale              IS
  'arcsec/pixel';

COMMENT ON COLUMN e_f2_lyot_wheel.plate_scale              IS
  'arcsec/nm';

COMMENT ON COLUMN e_f2_read_mode.minimum_exposure_time     IS
  'ms';

COMMENT ON COLUMN e_f2_read_mode.recommended_exposure_time IS
  'ms';

COMMENT ON COLUMN e_f2_read_mode.read_noise                IS
  'e- @ 77K';

COMMENT ON COLUMN e_f2_read_mode.readout_time              IS
  'ms';

COMMENT ON COLUMN e_gmos_custom_slit_width.width           IS
  'arcsec';

COMMENT ON COLUMN e_gmos_detector.north_pixel_size         IS
  'arcsec/pixel';

COMMENT ON COLUMN e_gmos_detector.south_pixel_size         IS
  'arcsec/pixel';

COMMENT ON COLUMN e_gmos_detector.shuffle_offset           IS
  'pixels';

COMMENT ON COLUMN e_gmos_detector.x_size                   IS
  'pixels';

COMMENT ON COLUMN e_gmos_detector.y_size                   IS
  'pixels';

COMMENT ON COLUMN e_gmos_detector.max_rois                 IS
  'maximum regions of interest';

COMMENT ON COLUMN e_gmos_disperser_order.count             IS
  'Corresponding numeric value.  E.g., 0, 1, 2';

COMMENT ON COLUMN e_gmos_dtax.dtax                         IS
  'Detector Translation Assembly X-Offset Value';

COMMENT ON COLUMN e_gmos_north_disperser.ruling_density    IS
  'lines/mm';

COMMENT ON COLUMN e_gmos_north_filter.wavelength           IS
  'µm';

COMMENT ON COLUMN e_gmos_north_fpu.slit_width              IS
  'arcsec';

COMMENT ON COLUMN e_gmos_south_disperser.ruling_density    IS
  'lines/mm';

COMMENT ON COLUMN e_gmos_south_filter.wavelength           IS
  'µm';

COMMENT ON COLUMN e_gmos_south_fpu.slit_width              IS
  'arcsec';

COMMENT ON COLUMN ephemeris_meta.horizons_soln_ref         IS
  'JPL Horizons ephemeris calc version';

COMMENT ON COLUMN gcal.exposure_time                       IS
  'ms';

COMMENT ON COLUMN gmos_custom_roi.x_min                    IS
  'pixel index';

COMMENT ON COLUMN gmos_custom_roi.y_min                    IS
  'pixel index';

COMMENT ON COLUMN gmos_custom_roi.x_range                  IS
  'pixel count';

COMMENT ON COLUMN gmos_custom_roi.y_range                  IS
  'pixel count';

COMMENT ON COLUMN gmos_nod_and_shuffle.a_offset_p          IS
  'arcsec, A offset position, offset in p';

COMMENT ON COLUMN gmos_nod_and_shuffle.a_offset_q          IS
  'arcsec, A offset position, offset in q';

COMMENT ON COLUMN gmos_nod_and_shuffle.b_offset_p          IS
  'arcsec, B offset position, offset in p';

COMMENT ON COLUMN gmos_nod_and_shuffle.b_offset_q          IS
  'arcsec, B offset position, offset in q';

COMMENT ON COLUMN gmos_nod_and_shuffle.e_offset            IS
  'whether to use electronic offsetting';

COMMENT ON COLUMN gmos_nod_and_shuffle.offset_rows         IS
  'detector rows';

COMMENT ON COLUMN gmos_nod_and_shuffle.cycles              IS
  'Nod and shuffle cycles';

COMMENT ON COLUMN smart_gmos_north.min_wavelength          IS
  'nm';

COMMENT ON COLUMN smart_gmos_north.max_wavelength          IS
  'nm';

COMMENT ON COLUMN smart_gmos_south.min_wavelength          IS
  'nm';

COMMENT ON COLUMN smart_gmos_south.max_wavelength          IS
  'nm';

COMMENT ON COLUMN step.location                            IS
  'Used to order steps, step.location values are sortable and it is always possible to insert new values between any two existing locations';

COMMENT ON COLUMN step.step_type                           IS
  'Step type distinguishes between various kinds of steps: bias, dark, science, smart GCAL, etc.';

COMMENT ON COLUMN step_f2.exposure_time                    IS
  'ms';

COMMENT ON COLUMN step_gcal.exposure_time                  IS
  'ms';

COMMENT ON COLUMN step_gmos_common.exposure_time           IS
  'ms';

COMMENT ON COLUMN step_gmos_north.wavelength               IS
  'nm';

COMMENT ON COLUMN step_gmos_south.wavelength               IS
  'nm';

COMMENT ON COLUMN step_science.offset_p                    IS
  'arcsec';

COMMENT ON COLUMN step_science.offset_q                    IS
  'arcsec';

COMMENT ON DOMAIN coadds                                   IS
  'Coadd count constrained to positive, non-zero';

COMMENT ON DOMAIN id_index                                 IS
  'Index of program and observation ids, constrained to positive, non-zero';

COMMENT ON DOMAIN identifier                               IS
  'Enumeration value constrained to a valid Scala id';

COMMENT ON DOMAIN milliseconds                             IS
  'Non-negative milliseconds';

COMMENT ON SEQUENCE static_config_static_id_seq            IS
  'Ever increasing id for static instrument configurations (see static_config table)';

COMMENT ON SEQUENCE step_step_id_seq                       IS
  'Ever increasing id for dynamic instrument configuration steps (see step table)';

COMMENT ON TABLE e_ephemeris_type                          IS
  'Type of ephemeris data / non-sidereal object, e.g. Comet';

COMMENT ON TABLE e_gmos_dtax                               IS
  'GMOS Detector Translation Assembly X-Offset [-6, 6]';

COMMENT ON TABLE e_gmos_e_offsetting                       IS
  'Values of a boolean flag determining whether to use electronic offsetting';

COMMENT ON TABLE e_gmos_roi                                IS
  'GMOS Detector Region of Interest';

COMMENT ON TABLE e_mos_preimaging                          IS
  'Values of a boolean flag determining whether an observation is for creating MOS masks';

COMMENT ON TABLE ephemeris_meta                            IS
  'Information used to determine whether an ephemeris update is necessary';

COMMENT ON TABLE gmos_custom_roi                           IS
  'GMOS detector custom region of interest description';

COMMENT ON TABLE smart_f2                                  IS
  'F2 instrument configuration key for Smart GCal lookups';

COMMENT ON TABLE smart_gmos_north                          IS
  'GMOS North instrument configuration key for Smart GCal lookups';

COMMENT ON TABLE smart_gmos_south                          IS
  'GMOS South instrument configuration key for Smart GCal lookups';

COMMENT ON TABLE static_config                             IS
  'Used to link an observation to its instrument''s static configuration';

COMMENT ON TABLE static_f2                                 IS
  'Unchangable F2 configuration for an observation';

COMMENT ON TABLE static_gmos_north                         IS
  'Unchangable GMOS North configuration for an observation';

COMMENT ON TABLE static_gmos_south                         IS
  'Unchangable GMOS South configuration for an observation';

COMMENT ON TABLE step                                      IS
  'Used to link an observation to each step of its instrument''s dynamic configuration';

COMMENT ON TABLE step_gmos_common                          IS
  'Common sequence step configuration across GMOS North and GMOS South';

COMMENT ON TYPE   evt_type                                 IS
  'Observing Event Type';
