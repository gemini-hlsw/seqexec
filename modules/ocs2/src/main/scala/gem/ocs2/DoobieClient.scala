// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import cats.effect.IO, cats.implicits._
import doobie._, doobie.implicits._
import gem.dao.meta._
import doobie.postgres.implicits._
import java.util.logging.{ Level, Logger }

/** Shared support for import applications using Doobie. */
trait DoobieClient extends ProgramIdMeta with ObservationIdMeta {

  val Url  = "jdbc:postgresql:gem"
  val User = "postgres"
  val Pass = ""

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", Url, User, Pass
  )

  val lxa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", Url, User, Pass
  )

  def configureLogging(): Unit = List(
    "edu.gemini.spModel.type.SpTypeUtil"
  ).map(Logger.getLogger).foreach(_.setLevel(Level.OFF))

  def ignoreUniqueViolation(fa: ConnectionIO[Int]): ConnectionIO[Int] =
    for {
      s <- HC.setSavepoint
      n <- fa.onUniqueViolation(HC.rollback(s).as(0)) guarantee HC.releaseSavepoint(s)
    } yield n

}
