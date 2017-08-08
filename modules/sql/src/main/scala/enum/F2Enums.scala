// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql
package enum

import doobie.imports._
import java.time.Duration
import shapeless.record._

object F2Enums {

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("F2Disperser", "Flamingos2 dispersers") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'wavelength -> Wavelength.Um`.T
        sql"SELECT id, id tag, short_name, long_name, wavelength FROM e_f2_disperser".query[(String, R)]
      },

      EnumDef.fromQuery("F2Filter", "Flamingos2 filters") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'wavelength -> Option[Double], 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, wavelength, obsolete FROM e_f2_filter".query[(String, R)]
      },

      EnumDef.fromQuery("F2FpUnit", "Flamingos2 focal plane units") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'slitWidth -> Int, 'decker -> String, 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, slit_width, decker, obsolete FROM e_f2_fpunit".query[(String, R)]
      },

      EnumDef.fromQuery("F2LyotWheel", "Flamingos2 Lyot wheel") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'plateScale -> Double, 'pixelScale -> Double, 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, plate_scale, pixel_scale, obsolete FROM e_f2_lyot_wheel".query[(String, R)]
      },

      EnumDef.fromQuery("F2ReadMode", "Flamingos2 read modes") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'description -> String, 'minimumExposureTime -> Duration, 'recommendedExposureTime -> Duration, 'readoutTime -> Duration, 'readCount -> Int, 'readNoise -> Double`.T
        sql"SELECT id, id tag, short_name, long_name, description, minimum_exposure_time, recommended_exposure_time, readout_time, read_count, read_noise FROM e_f2_read_mode".query[(String, R)]
      },

      EnumDef.fromQuery("F2WindowCover", "Flamingos2 window cover state") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"SELECT id, id tag, short_name, long_name FROM e_f2_window_cover".query[(String, R)]
      }

    )

}
