// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import cats.implicits._
import doobie._
import doobie.postgres.implicits._
import gem.util.Location

trait LocationMeta {

  implicit val LocationMeta: Meta[Location.Middle] =
    Meta[List[Int]].xmap(Location.unsafeMiddleFromFoldable(_), _.toList)

}
object LocationMeta extends LocationMeta
