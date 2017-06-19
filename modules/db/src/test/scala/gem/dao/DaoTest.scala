package gem.dao

import doobie.imports._
import gem.{Program, Step}

import scalaz.effect.IO

/** Base trait for DAO test cases.
  */
trait DaoTest extends gem.Arbitraries {
  val pid = Program.Id.parse("GS-1234A-Q-1")

  private val xa = Transactor.after.set(
    DriverManagerTransactor[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:gem",
      "postgres",
      ""
    ),
    HC.rollback
  )

  def withProgram[A](test: ConnectionIO[A]): A =
    (for {
      _ <- ProgramDao.insert(Program(pid, "Test Prog", List.empty[Step[Nothing]]))
      a <- test
    } yield a).transact(xa).unsafePerformIO()
}
