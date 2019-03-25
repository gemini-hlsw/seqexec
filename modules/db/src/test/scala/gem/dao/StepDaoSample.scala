// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import doobie._
import gem.{ Observation, Step }
import gem.util.Location
import scala.collection.immutable.TreeMap

object StepDaoSample extends TimedSample {
  type Result = TreeMap[Location.Middle, Step]

  val oid: Observation.Id = Observation.Id.unsafeFromString("GS-2016A-Q-102-108")

  override def runl(args: List[String]): ConnectionIO[Result] =
    StepDao.selectAll(oid)

  override def format(r: Result): String =
    r.toList.map { case (l, s) => f"$l%10s -> $s" }.mkString(", \n")
}
