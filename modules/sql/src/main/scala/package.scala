// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import doobie.imports._
import java.time.{ Duration, ZoneId }

package object sql {

  implicit val DurationMeta: Meta[Duration] =
    Meta[Long].xmap(Duration.ofMillis, _.toMillis)

  implicit val AngleMeta: Meta[Angle] =
    Meta[Double].xmap(Angle.fromArcsecs, _.toArcsecs)

  implicit val ZoneIdMeta: Meta[ZoneId] =
    Meta[String].xmap(ZoneId.of, _.toString)

}
