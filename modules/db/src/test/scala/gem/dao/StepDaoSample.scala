package gem.dao

import gem.{Location, Observation, Step}
import gem.config.InstrumentConfig

import doobie.imports._

import scalaz.==>>

object StepDaoSample extends TimedSample {
  type Result = Location.Middle ==>> Step[InstrumentConfig]

  val oid = Observation.Id.unsafeFromString("GS-2016A-Q-102-108")

  override def runl(args: List[String]): ConnectionIO[Result] =
    StepDao.selectAll(oid)

  override def format(r: Result): String =
    r.toAscList.map { case (l, s) => f"$l%10s -> $s" }.mkString(", \n")
}
