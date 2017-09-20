// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql
package enum

import doobie._, doobie.implicits._
import shapeless.record._

object TargetEnums {

  val enums: List[ConnectionIO[EnumDef]] =
    List(

      EnumDef.fromQuery("TrackType", "track types") {
        type R = Record.`'tag -> String`.T
        sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'e_track_type'
         """.query[(String, R)]
      }

    )

}
