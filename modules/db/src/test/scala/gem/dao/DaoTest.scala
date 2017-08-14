// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.effect.IO
import doobie.imports._
import gem.{ Program, Step }

/** Base trait for DAO test cases.
  */
trait DaoTest extends gem.Arbitraries {
  val pid = Program.Id.unsafeFromString("GS-1234A-Q-1")

  protected val xa = Transactor.after.set(
    Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:gem",
      "postgres",
      ""
    ),
    HC.rollback
  )

  def withProgram[A](test: ConnectionIO[A]): A =
    (for {
      _ <- ProgramDao.insertFlat(Program(pid, "Test Prog", List.empty[Step[Nothing]]))
      a <- test
    } yield a).transact(xa).unsafeRunSync()
    
}
