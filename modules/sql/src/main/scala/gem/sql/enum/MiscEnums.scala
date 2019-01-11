// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql
package enum

import doobie._
import doobie.implicits._
import java.time.ZoneId

import gem.sql.EnumDef
import shapeless.record._

object MiscEnums {
  import Angle._

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("MosPreImaging", "MOS pre-imaging category") {
        type R = Record.`'tag -> String, 'description -> String, 'toBoolean -> Boolean`.T
        sql"SELECT id, id tag, description, to_boolean FROM e_mos_preimaging".query[(String, R)]
      },

      EnumDef.fromQuery("StepType", "step types") {
        type R = Record.`'tag -> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'step_type'
         """.query[(String, R)]
      },

      EnumDef.fromQuery("EventType", "observe [[gem.Event Event]]) types") {
        type R = Record.`'tag -> String`.T
        sql"""
          SELECT enumlabel a, enumlabel b
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'evt_type'
        """.query[(String, R)]
      },

      EnumDef.fromQuery("Instrument", "instruments") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_instrument".query[(String, R)]
      },

      EnumDef.fromQuery("ProgramType", "program types (see [[gem.ProgramId ProgramId]])") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        sql"SELECT id, id tag, short_name, long_name, obsolete FROM e_program_type".query[(String, R)]
      },

      EnumDef.fromQuery("Site", "Gemini observing sites") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'mountain -> String, 'latitude -> Degrees, 'longitude -> Degrees, 'altitude -> Int, 'timezone -> ZoneId`.T
        sql"""SELECT id, id tag, short_name, long_name, mountain, latitude, longitude, altitude,
                timezone
              FROM e_site""".query[(String, R)]
      },

      EnumDef.fromQuery("ProgramRole", "user roles with respect to a given program") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"SELECT id, id tag, short_name, long_name FROM e_program_role".query[(String, R)]
      },

      EnumDef.fromQuery("Half", "semester half") {
        type R = Record.`'tag -> String, 'toInt -> Int`.T
        sql"""
          SELECT enumlabel x, enumlabel y, enumsortorder - 1
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'half'
         """.query[(String, R)]
      },

      EnumDef.fromQuery("EphemerisKeyType", "Non-sidereal target lookup type") {
        type R = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        sql"SELECT id, id tag, short_name, long_name FROM e_ephemeris_type".query[(String, R)]
      },

      EnumDef.fromQuery("KeywordName", "Fits Keyword names") {
        type R = Record.`'tag -> String, 'name -> String`.T
        sql"SELECT id, id tag, name FROM e_fits_keyword_names".query[(String, R)]
      },

      EnumDef.fromQuery("DhsKeywordName", "DHS Keyword names") {
        type R = Record.`'tag -> String, 'keyword -> KeywordName, 'name -> String`.T
        sql"SELECT keyword, keyword tag, keyword tag, name FROM e_dhs_keyword_names".query[(String, R)]
      }
    )

}
