package gem.dao

import gem.{Observation, Step}
import gem.config.InstrumentConfig
import gem.enum.Instrument

import doobie.imports._

//import scalaz._, Scalaz._

object StepDaoSample extends TimedSample {
  type Result = List[Step[InstrumentConfig]]

  val oid = Observation.Id.unsafeFromString("GS-2016A-Q-102-108")

  override def runl(args: List[String]): ConnectionIO[Result] =
    StepDao.selectAll(oid, Instrument.Flamingos2)

  override def format(r: Result): String =
    r.mkString(", \n")
}
