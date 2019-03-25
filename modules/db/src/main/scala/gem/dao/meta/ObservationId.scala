// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.Observation

trait ObservationIdMeta {

  // Observation.Id as string
  implicit val ObservationIdMeta: Meta[Observation.Id] =
    Meta[String].timap(Observation.Id.unsafeFromString)(_.format)

}
object ObservationIdMeta extends ObservationIdMeta
