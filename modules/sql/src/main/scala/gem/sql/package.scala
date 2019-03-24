// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import doobie._
import java.time.{ Duration, ZoneId }

package object sql {
  implicit val DurationMeta: Meta[Duration] =
    Meta[Long].timap(Duration.ofMillis)(_.toMillis)

  implicit val ZoneIdMeta: Meta[ZoneId] =
    Meta[String].timap(ZoneId.of)(_.toString)
}
