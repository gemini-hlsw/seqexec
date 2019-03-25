// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql
package enum

import doobie._, doobie.implicits._
import shapeless.record._

object GcalEnums {

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("GcalFilter", "calibration unit filter") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_gcal_filter".query[(String, R)]
      },

      EnumDef.fromQuery("GcalContinuum", "calibration unit continuum lamps") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_gcal_continuum".query[(String, R)]
      },

      EnumDef.fromQuery("GcalArc", "calibration unit arc lamps") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_gcal_arc".query[(String, R)]
      },

      EnumDef.fromQuery("GcalDiffuser", "calibration unit diffusers") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_gcal_diffuser".query[(String, R)]
      },

      EnumDef.fromQuery("GcalShutter", "calibration unit shutter states") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_gcal_shutter".query[(String, R)]
      },

      EnumDef.fromQuery("GcalBaselineType", "calibration baseline type") {
        type R = Record.`'tag -> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'gcal_baseline_type'
        """.query[(String, R)]
      },

      EnumDef.fromQuery("GcalLampType", "calibration lamp type") {
        type R = Record.`'tag -> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'gcal_lamp_type'
        """.query[(String, R)]
      },

      EnumDef.fromQuery("SmartGcalType", "\"smart\" calibration sequence tpes") {
        type R = Record.`'tag -> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'smart_gcal_type'
        """.query[(String, R)]
      }

    )

}
