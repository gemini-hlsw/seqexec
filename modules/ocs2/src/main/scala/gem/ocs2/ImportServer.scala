// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import Decoders._

import cats.effect.IO, cats.implicits._

import gem.{ Dataset, Observation, Program }
import gem.ocs2.pio.{ PioDecoder, PioError }
import gem.ocs2.pio.PioError._

import fs2.{ Stream, StreamApp }

import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client.Client
import org.http4s.client.blaze.Http1Client
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.scalaxml.xml

import java.net.URLEncoder
import java.util.logging.Logger

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.xml.{Elem, Node}

/** A server that accepts HTTP requests to import observations or programs from
  * an OCS2 ODB.  If the corresponding observation or program has already been
  * loaded, it is deleted and wholly replaced by the latest version from the
  * ODB.
  */
final class ImportServer(ocsHost: String) {

  import ImportServer._

  private val client: Stream[IO, Client[IO]] =
    Http1Client.stream[IO]()

  private def uri(id: String): String =
    s"${fetchServiceUrl(ocsHost)}/${URLEncoder.encode(id, "UTF-8")}"

  private def badRequest(id: String, idType: String): IO[Response[IO]] =
    BadRequest(s"Could not parse $idType id '$id'")

  private def fetchDecodeAndStore[A](id: String, f: (A, List[Dataset]) => IO[Unit])(implicit ev: PioDecoder[A]): IO[Response[IO]] = {
    def decodeAndStore(xml: Node): IO[Response[IO]] =
      PioDecoder[(A, List[Dataset])].decode(xml).leftMap(_.toResponse(id)).traverse { case (a, ds) =>
        f(a, ds).as(Ok(s"Imported $id"))
      }.flatMap(_.merge)

    // TODO: add timeout
    val io: IO[Either[Throwable, Response[IO]]] =
      client.flatMap { c =>
        Stream.eval {
          c.expect[Elem](uri(id))
          .flatMap(decodeAndStore)
          .attempt
        }
      }.compile.last.map(_.getOrElse(Left(new RuntimeException("Impossible: empty stream"))))

    io.unsafeRunSync match {
      case Right(r) => IO.pure(r)
      case Left(ex) => InternalServerError(s"Problem importing '$id': ${ex.getMessage}")
    }

  }

  def importObservation(obsIdStr: String): IO[Response[IO]] = {
    val checkId = Observation.Id.fromString(obsIdStr) toRight badRequest(obsIdStr, "observation")
    checkId.map { oid => fetchDecodeAndStore[Observation.Full](obsIdStr, (a, ds) => Importer.importObservation(oid, a, ds)) }.merge
  }

  def importProgram(pidStr: String): IO[Response[IO]] = {
    val checkId = Either.catchOnly[IllegalArgumentException](Program.Id.unsafeFromString(pidStr))
                    .leftMap(_ => badRequest(pidStr, "program"))
    checkId.as { fetchDecodeAndStore[Program[Observation.Full]](pidStr, Importer.importProgram) }.merge
  }
}

object ImportServer extends StreamApp[IO] {
  private val Log = Logger.getLogger(ImportServer.getClass.getName)

  // Port where our http service will run.
  val port: Int = 8989

  // How long to wait for the import to complete before giving up.
  val timeout: Duration = 30 seconds

  private def fetchServiceUrl(hostName: String): String =
    s"http://$hostName:8442/ocs3/fetch"

  implicit class PioErrorSyntax(e: PioError) {
    def toResponse(id: String): IO[Response[IO]] = {
      val msg = e match {
        case MissingKey(name)            => s"missing '$name'"
        case ParseError(value, dataType) => s"could not parse '$value' as '$dataType'"
      }
      InternalServerError(s"Error parsing $id: $msg")
    }
  }


  def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, StreamApp.ExitCode] = {

    val hostName = args match {
      case Nil       => "localhost"
      case host :: _ => host
    }

    Log.info(s"Starting import server on port $port connecting to ${fetchServiceUrl(hostName)}")

    val importServer = new ImportServer(hostName)

    val service = HttpService[IO] {
      case GET -> Root / "obs" / obsId =>
        importServer.importObservation(obsId)

      case GET -> Root / "prog" / progId =>
        importServer.importProgram(progId)
    }

    BlazeBuilder[IO]
      .bindHttp(8989, "localhost")
      .mountService(service, "/import")
      .serve

  }
}
