package gem.dao

import doobie.imports._
import gem.{Observation, Program, Step}
import gem.config._
import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._

import scalaz.effect.IO


class ObservationDaoSpec extends PropSpec with PropertyChecks with gem.Arbitraries {
  import ObservationDaoSpec._

  property("ObservationDao should roundtrip complete observations") {
    val oid = Observation.Id(pid, 1)

    forAll(arbObservation(oid).arbitrary) { obsIn =>
      val obsOut = doTest {
        for {
          _ <- ObservationDao.insert(obsIn)
          o <- ObservationDao.select(oid)
        } yield o
      }

      obsOut shouldEqual obsIn
    }
  }
}

object ObservationDaoSpec {
  private val pid = Program.Id.parse("GS-1234A-Q-1")

  private val xa  = DriverManagerTransactor[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  val static = F2StaticConfig(mosPreImaging = false)

  private def doTest[A](test: ConnectionIO[A]): A =
    (for {
      _ <- ProgramDao.insert(Program(pid, "ObservationDaoSpec Prog", List.empty[Step[Nothing]]))
      a <- test
      _ <- sql"""DELETE FROM program WHERE program_id = $pid""".update.run
    } yield a).transact(xa).unsafePerformIO()
}