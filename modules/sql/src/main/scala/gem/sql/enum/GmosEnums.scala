// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql
package enum

import doobie._, doobie.implicits._
import shapeless.record._

object GmosEnums {
  import Angle.Arcseconds

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("GmosAdc", "GMOS ADC") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_adc""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosAmpCount", "GMOS amp count") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_amp_count""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosAmpGain", "GMOS amp gain") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_amp_gain""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosAmpReadMode", "GMOS amp read mode") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_amp_read_mode""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosCustomSlitWidth", "GMOS custom slit width") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'width -> Arcseconds`.T
        sql"""SELECT id, id tag, short_name, long_name, width FROM e_gmos_custom_slit_width""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosDetector", "GMOS detector") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'northPixelSize -> Arcseconds, 'southPixelSize -> Arcseconds, 'shuffleOffset -> Int, 'xSize -> Int, 'ySize -> Int, 'maxRois -> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, north_pixel_size, south_pixel_size, shuffle_offset, x_size, y_size, max_rois FROM e_gmos_detector""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosDisperserOrder", "GMOS disperser order") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'count -> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, count FROM e_gmos_disperser_order""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosDtax", "GMOS detector translation X offset") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'dtax -> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, dtax FROM e_gmos_dtax""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosEOffsetting", "GMOS Electric Offsetting") {
        type R = Record.`'tag -> String, 'description -> String, 'toBoolean -> Boolean`.T
        sql"select id, id tag, description, to_boolean FROM e_gmos_e_offsetting".query[(String, R)]
      },

      EnumDef.fromQuery("GmosNorthDisperser", "GMOS North dispersers") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'rulingDensity -> Int, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, ruling_density, obsolete FROM e_gmos_north_disperser""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosNorthFilter", "GMOS North filters") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'wavelength -> Wavelength.Um, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, wavelength, obsolete FROM e_gmos_north_filter""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosNorthFpu", "GMOS North focal plane units") {
        type GmosNorthFpuRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'slitWidth -> Option[Arcseconds]`.T
        sql"""SELECT id, id tag, short_name, long_name, slit_width FROM e_gmos_north_fpu""".query[(String, GmosNorthFpuRec)]
      },

      EnumDef.fromQuery("GmosNorthStageMode", "GMOS North stage modes") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gmos_north_stage_mode""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosRoi", "GMOS ROI (region of interest)") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gmos_roi""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosSouthDisperser", "GMOS South dispersers") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'rulingDensity -> Int, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, ruling_density, obsolete FROM e_gmos_south_disperser""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosSouthFilter", "GMOS South filters") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'wavelength -> Wavelength.Um, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, wavelength, obsolete FROM e_gmos_south_filter""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosSouthFpu", "GMOS South focal plane units") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'slitWidth -> Option[Arcseconds]`.T
        sql"""SELECT id, id tag, short_name, long_name, slit_width FROM e_gmos_south_fpu""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosSouthStageMode", "GMOS South stage mode") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gmos_south_stage_mode""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosXBinning", "GMOS X-binning") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'count -> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, count FROM e_gmos_binning""".query[(String, R)]
      },

      EnumDef.fromQuery("GmosYBinning", "GMOS Y-binning") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'count -> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, count FROM e_gmos_binning""".query[(String, R)]
      }

    )

}
