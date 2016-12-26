package gem.dao

import gem.{Location, Observation, Step}
import gem.config.InstrumentConfig

import doobie.imports._

object StepDaoSample extends TimedSample {
  type Result = List[(Location.Middle, Step[InstrumentConfig])]

  val oid = Observation.Id.unsafeFromString("GS-2016A-Q-102-108")

  override def runl(args: List[String]): ConnectionIO[Result] =
    StepDao.selectAll(oid)

  override def format(r: Result): String =
    r.mkString(", \n")
}
