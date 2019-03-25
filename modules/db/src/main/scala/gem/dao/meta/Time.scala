// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.util.Timestamp
import java.time.{ Instant, Duration }

trait TimeMeta {

  implicit val TimestampMeta: Meta[Timestamp] =
    Meta[Instant].timap(Timestamp.unsafeFromInstant)(_.toInstant)

  implicit val DurationMeta: Meta[Duration] =
    Distinct.long("milliseconds").timap(Duration.ofMillis)(_.toMillis)

}
object TimeMeta extends TimeMeta
