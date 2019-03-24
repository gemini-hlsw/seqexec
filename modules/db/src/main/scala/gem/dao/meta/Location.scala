// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import cats.implicits._
import doobie._
import doobie.postgres.implicits._
import gem.util.Location

trait LocationMeta {

  implicit val LocationGet: Get[Location.Middle] =
    Get[List[Int]].map(Location.unsafeMiddleFromFoldable(_))

  implicit val LocationPut: Put[Location.Middle] =
    Put[List[Int]].contramap(_.toList)

}
object LocationMeta extends LocationMeta
