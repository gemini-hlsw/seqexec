// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.util.Timestamp
import java.time.{ Instant, Duration }

trait TimeMeta {

  implicit val TimestampMeta: Meta[Timestamp] =
    Meta[Instant].xmap(Timestamp.unsafeFromInstant, _.toInstant)

  implicit val DurationMeta: Meta[Duration] =
    Distinct.long("milliseconds").xmap(Duration.ofMillis, _.toMillis)

}
object TimeMeta extends TimeMeta
