// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.web

import gem.{ Log, Observation, Program, User, Service => GemService, TargetEnvironment }
import gem.config.StaticConfig
import gem.dao.{ DatabaseConfiguration, ObservationDao, ProgramDao, UserDao }
import gem.enum.ProgramRole
import gem.json.instances.all._
import gem.math.Index
import gem.syntax.prism._

import cats.effect.{ ContextShift, IO }
import cats.implicits._

import doobie._
import doobie.free.connection.setAutoCommit
import doobie.implicits._
import doobie.util.invariant.UnexpectedEnd

import io.circe.generic.auto._
import io.circe.syntax._

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._

import org.scalatest._

import scala.collection.immutable.{ SortedSet, TreeMap }
import scala.concurrent.ExecutionContext

/**
 * Test cases for gem.web.Application.
 */
class ApplicationSpec extends FlatSpec with Matchers {

  // Constants used in setting up the database for test cases.
  private object Setup {

    val pid: Program.Id =
      Program.Id.fromString.unsafeGet("GS-1234A-Q-1")

    val prog: Program =
      Program(pid, "Test Prog", TreeMap.empty)

    def oid(index: Short): Observation.Id =
      Observation.Id(pid, Index.fromShort.getOption(index).getOrElse(sys.error("expecting a positive short")))

    val oid1: Observation.Id =
      oid(1)

    def formatOid(index: Short): String =
      oid(index).format

    val obs: Observation =
      Observation.Phoenix(
        "Test Obs",
        TargetEnvironment.Phoenix(None, SortedSet.empty),
        StaticConfig.Phoenix(),
        Nil
      )

    val user: User[ProgramRole] =
      User[ProgramRole]("gprincip", "Gavrilo", "Princip", "gprincip@youngbosnia.com", false, Map.empty)

    val addProgram: ConnectionIO[Unit] =
      ProgramDao.insertFlat(prog).void

    val addObservation: ConnectionIO[Unit] =
      ObservationDao.insert(oid1, obs)

    val addUser: ConnectionIO[Unit] =
      UserDao.insertUser(user, "pass")

    val addRoles: ConnectionIO[Unit] =
      UserDao.setRole(user.id, pid, ProgramRole.PI) *>
        UserDao.setRole(user.id, pid, ProgramRole.GEM)

    val addProgramAndUser: ConnectionIO[Unit] =
      addProgram *> addUser *> addRoles

    val addProgramUserAndObs: ConnectionIO[Unit] =
      addProgram *> addUser *> addRoles *> addObservation

  }

  // Executes an Application query with the given path and query string, having
  // first setup the database with the provided `setup`.  Rolls back all changes
  // when finished.
  private def runQuery(
    path:  String,
    query: String
  )(
    setup: ConnectionIO[Unit]
  ): Response[IO] = {

    implicit val ioContextShift: ContextShift[IO] =
      IO.contextShift(ExecutionContext.global)

    val xa: Transactor[IO] =
      Transactor.before.set(
        Transactor.after.set(
          DatabaseConfiguration.forTesting.transactor[IO],
          HC.rollback
        ),
        setAutoCommit(false) *> setup
      )

    val test: IO[Option[Response[IO]]] =
      for {
        s <- Log.newLog[IO]("ApplicationSpec", xa).map(GemService(xa, _, Setup.user))
        q  = AuthedRequest(s, Request[IO](uri = Uri(path = path, query=org.http4s.Query.fromString(query))))
        r <- Application.service[IO].apply(q).value
      } yield r

    test.unsafeRunSync().getOrElse(sys.error("you supplied a query that doesn't match"))

  }

  "Application.service" should "list nothing if empty" in {
    val res = runQuery("/api/query/program", "query=*") {
      Setup.addUser
    }

    res.as[String].unsafeRunSync shouldEqual Ok(List.empty[(Program.Id, String)].asJson).unsafeRunSync.as[String].unsafeRunSync
  }

  it should "list available programs" in {
    val res = runQuery("/api/query/program", "query=*") {
      Setup.addProgramAndUser
    }

    res.as[String].unsafeRunSync shouldEqual Ok(List((Setup.pid, "Test Prog")).asJson).unsafeRunSync.as[String].unsafeRunSync
  }

  it should "fetch an existing observation" in {
    val res = runQuery(s"api/fetch/obs/${Setup.oid1.format}", "") {
      Setup.addProgramUserAndObs
    }

    res.as[String].unsafeRunSync shouldEqual Ok(Setup.obs.asJson).unsafeRunSync.as[String].unsafeRunSync
  }

  it should "fail to fetch a missing observation" in {
    assertThrows[UnexpectedEnd.type] {
      runQuery(s"api/fetch/obs/${Setup.oid1.format}", "") {
        Setup.addProgramAndUser
      }
    }
  }

  it should "query an existing observation" in {
    val res = runQuery(s"api/query/obs/${Setup.oid1.format}", "") {
      Setup.addProgramUserAndObs
    }

    res.as[String].unsafeRunSync shouldEqual Ok(Setup.obs.asJson).unsafeRunSync.as[String].unsafeRunSync
  }

  it should "query a missing observation" in {
    val res = runQuery(s"api/query/obs/${Setup.oid1.format}", "") {
      Setup.addProgramAndUser
    }

    res.as[String].unsafeRunSync shouldEqual NotFound().unsafeRunSync.as[String].unsafeRunSync
  }
}
