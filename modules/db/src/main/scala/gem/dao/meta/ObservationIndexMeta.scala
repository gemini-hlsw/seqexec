// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.math.Index

trait ObservationIndexMeta {

  // Index has a DISTINCT type due to its check constraint so we
  // need a fine-grained mapping here to satisfy the query checker.
  implicit val ObservationIndexMeta: Meta[Index] =
    Distinct.short("id_index").xmap(Index.unsafeFromShort, _.toShort)

}
object ObservationIndexMeta extends ObservationIndexMeta

