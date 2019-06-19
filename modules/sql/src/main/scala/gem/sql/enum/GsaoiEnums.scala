// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql
package enum

import doobie._, doobie.implicits._
import shapeless.record._
import shapeless.Witness
import gem.sql.FiniteDuration

object GsaoiEnums {
  import EnumRefs._

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("GsaoiReadMode", "GSAOI Read Mode") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'ndr -> Int, 'readNoise -> Int, 'minimumExposureTime -> FiniteDuration.Seconds, 'overhead -> FiniteDuration.Seconds`.T
        sql"""SELECT id, id tag, short_name, long_name, ndr, read_noise, minimum_exposure_time, overhead FROM e_gsaoi_read_mode""".query[(String, E)]
      },

      EnumDef.fromQuery("GsaoiFilter", "GSAOI Filter") {
        val  m = Witness('MagnitudeBand)
        val  r = Witness('GsaoiReadMode)
        type M = m.T
        type R = r.T
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'wavelength -> Wavelength.Um, 'readMode -> EnumRef[R], 'exposureTime5050 -> FiniteDuration.Seconds, 'exposureTimeHalfWell -> FiniteDuration.Seconds, 'band -> Option[EnumRef[M]]`.T
        val ret = sql"""SELECT id, id tag, short_name, long_name, wavelength, read_mode_id, exposure_time_50_50, exposure_time_half_well, band_id FROM e_gsaoi_filter""".query[(String, E)]
        (ret, m.value: M, r.value: R)._1 // convince scalac that we really do use M and R
      },

      EnumDef.fromQuery("GsaoiUtilityWheel", "Gsaoi Utility Wheel") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gsaoi_utility_wheel""".query[(String, E)]
      },

      EnumDef.fromQuery("GsaoiRoi", "Gsaoi Region of Interest") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gsaoi_roi""".query[(String, E)]
      },

      EnumDef.fromQuery("GsaoiOdgwSize", "Gsaoi ODGW Size") {
        type E = Record.`'tag -> String, 'pixels -> Int`.T
        sql"""SELECT id, id tag, pixels FROM e_gsaoi_odgw_pixels""".query[(String, E)]
      }
    )

}
