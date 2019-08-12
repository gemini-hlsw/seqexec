// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql
package enum

import doobie._, doobie.implicits._
import shapeless.record._
import shapeless.Witness

object GpiEnums {
  import EnumRefs._

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("GpiAdc", "GPI ADC") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'value -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, value FROM e_gpi_adc""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiFilter", "GPI Filter") {
        val  m = Witness('MagnitudeBand)
        type M = m.T
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'band -> EnumRef[M], 'obsolete -> Boolean`.T
        val ret = sql"""SELECT id, id tag, short_name, long_name, band, obsolete FROM e_gpi_filter""".query[(String, E)]
        (ret, m.value: M)._1 // convince scalac that we really do use M
      },

      EnumDef.fromQuery("GpiDisperser", "GPI Disperser") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"""SELECT id, id tag, short_name, long_name FROM e_gpi_disperser""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiApodizer", "GPI Apodizer") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gpi_apodizer""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiLyot", "GPI Lyot") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gpi_lyot""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiASU", "GPI Artificial Source Unit") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'value -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, value FROM e_gpi_artificial_source_unit""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiEntranceShutter", "GPI Entrance Shutter") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'value -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, value FROM e_gpi_entrance_shutter""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiScienceArmShutter", "GPI Science Arm Shutter") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'value -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, value FROM e_gpi_sience_arm_shutter""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiCalEntranceShutter", "GPI Cal Entrance Shutter") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'value -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, value FROM e_gpi_cal_entrance_shutter""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiReferenceArmShutter", "GPI Reference Arm Shutter") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'value -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, value FROM e_gpi_reference_arm_shutter""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiPupilCamera", "GPI Pupil Camera") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'value -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, value FROM e_gpi_pupil_camera""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiFPM", "GPI FPM") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gpi_fpm""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiSamplingMode", "GPI Sampling Mode") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gpi_sampling_mode""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiCassegrain", "GPI Cassegrain") {
        type E = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'value -> Int`.T
        sql"""SELECT id, id tag, short_name, long_name, value FROM e_gpi_cassegrain""".query[(String, E)]
      },

      EnumDef.fromQuery("GpiObservingMode", "GPI ObservingMode") {
        val (a, b, c, d, e) = (Witness('GpiFilter), Witness('GpiApodizer), Witness('GpiFPM), Witness('GpiLyot), Witness('GpiObservingMode))
        type A = a.T
        type B = b.T
        type C = c.T
        type D = d.T
        type E = e.T
        type F = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'filter -> Option[EnumRef[A]], 'filterIterable -> Boolean, 'apodizer -> Option[EnumRef[B]], 'fpm -> Option[EnumRef[C]], 'lyot -> Option[EnumRef[D]], 'brightLimitPrism -> Option[MagnitudeValue], 'brightLimitWollaston -> Option[MagnitudeValue], 'correspondingHMode -> LazyEnumRef[E], 'obsolete  -> Boolean`.T
        val ret = sql"""SELECT id, id tag, short_name, long_name, filter, filter_iterable, apodizer, fpm, lyot, bright_limit_prism, bright_limit_wollaston, corresponding_h_mode, obsolete FROM e_gpi_observing_mode""".query[(String, F)]
        (ret, a.value: A, b.value: B, c.value: C, d.value: D, e.value: E)._1 // suppress unused warnigs
      },

      EnumDef.fromQuery("GpiReadMode", "GPI ReadMode") {
        type E = Record.`'tag -> String, 'longName -> String, 'value -> Int`.T
        sql"""SELECT id, id tag, long_name, value FROM e_gpi_read_mode""".query[(String, E)]
      }

    )

}
