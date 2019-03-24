// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package ocs2

import cats.effect.{ ContextShift, IO, IOApp, ExitCode }
import cats.implicits._
import gem.dao.DatabaseConfiguration
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.Router
import org.http4s.implicits._

import java.util.logging.Logger

/** A server that accepts HTTP requests to import observations or programs from
  * an OCS2 ODB.  If the corresponding observation or program has already been
  * loaded, it is deleted and wholly replaced by the latest version from the
  * ODB.
  */
final class ImportServer(ocsHost: String)(implicit ev: ContextShift[IO]) {

  private val xa = DatabaseConfiguration.forTesting.transactor[IO]

  private def badRequest(id: String, idType: String): IO[Response[IO]] =
    BadRequest(s"Could not parse $idType id '$id'")

  private def importRemote[A](
    idStr:     String,
    parseFun:  String => Option[A],
    typeName:  String,
    importFun: A => IO[Either[String, Unit]]
  ): IO[Response[IO]] =
    parseFun(idStr).toRight(badRequest(idStr, typeName)).map { id =>
      importFun(id).map {
        case Left(msg) => InternalServerError(s"Error parsing $idStr: $msg")
        case Right(_)  => Ok(s"Imported $idStr")
      }.flatten
    }.merge

  def importObservation(obsIdStr: String): IO[Response[IO]] =
    importRemote[Observation.Id](
      obsIdStr,
      Observation.Id.fromString,
      "observation",
      OdbClient.importObservation(ocsHost, _, xa)
    )

  def importProgram(pidStr: String): IO[Response[IO]] = {
    importRemote[Program.Id](
      pidStr,
      ProgramId.fromString.getOption,
      "program",
      OdbClient.importProgram(ocsHost, _, xa)
    )
  }
}

object ImportServer extends IOApp {
  private val Log = Logger.getLogger(ImportServer.getClass.getName)

  // Port where our http service will run.
  val port: Int = 8989

  def run(args: List[String]): IO[ExitCode] = {

    val hostName = args match {
      case Nil       => "localhost"
      case host :: _ => host
    }

    Log.info(s"Starting import server on port $port connecting to $hostName")

    val importServer = new ImportServer(hostName)

    val service = HttpRoutes.of[IO] {
      case GET -> Root / "obs" / obsId =>
        importServer.importObservation(obsId)

      case GET -> Root / "prog" / progId =>
        importServer.importProgram(progId)
    }

    val httpApp = Router[IO]("/import" -> service).orNotFound

    BlazeServerBuilder[IO]
      .bindHttp(8989, "localhost")
      .withHttpApp(httpApp)
      .resource.use(_ => IO.never)
      .start
      .as(ExitCode.Success)

  }
}
