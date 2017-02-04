package gem.seqimporter

import Decoders._

import edu.gemini.spModel.core.SPProgramID

import gem.{Dataset, Observation, Program, Step}
import gem.config.InstrumentConfig
import gem.seqimporter.pio.{PioDecoder, PioError}
import gem.seqimporter.pio.PioError._
import org.http4s.{EntityEncoder, HttpService, Response, Status}
import org.http4s.Status.{BadRequest, InternalServerError, Ok}
import org.http4s.client.blaze.PooledHttp1Client
import org.http4s.dsl._
import org.http4s.headers.`Content-Length`
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.scalaxml.xml

import java.net.URLEncoder
import java.util.logging.Logger

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.xml.{Elem, Node}

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

/** A server that accepts HTTP requests to import observations or programs from
  * an OCS2 ODB.  If the corresponding observation or program has already been
  * loaded, it is deleted and wholly replaced by the latest version from the
  * ODB.
  */
final class ImportServer(ocsHost: String) {

  import ImportServer._

  private val client = PooledHttp1Client()

  private def uri(id: String): String =
    s"${fetchServiceUrl(ocsHost)}/${URLEncoder.encode(id, "UTF-8")}"

  private def badRequest(id: String, idType: String): ServerResponse =
    ServerResponse(BadRequest, s"Could not parse $idType id '$id'")

  private def fetchDecodeAndStore[A](id: String, f: (A, List[Dataset]) => Task[Unit])(implicit ev: PioDecoder[A]): ServerResponse = {
    def decodeAndStore(xml: Node): Task[ServerResponse] =
      (for {
        a  <- PioDecoder[A].decode(xml).leftMap(_.toResponse(id))
        ds <- DatasetsDecoder.decode(xml).leftMap(_.toResponse(id))
      } yield f(a, ds).as(ServerResponse(Ok, s"Imported $id"))).sequenceU.map(_.merge)

    client.expect[Elem](uri(id))
          .flatMap(decodeAndStore)
          .unsafePerformSyncAttemptFor(timeout)
          .leftMap { ex => ServerResponse(InternalServerError, s"Problem importing '$id': ${ex.getMessage}") }
          .merge
  }

  def importObservation(obsIdStr: String): ServerResponse = {
    val checkId = Observation.Id.fromString(obsIdStr) \/> badRequest(obsIdStr, "observation")
    checkId.as { fetchDecodeAndStore[Obs](obsIdStr, Importer.importObservation) }.merge
  }

  def importProgram(pidStr: String): ServerResponse = {
    val checkId = \/.fromTryCatchNonFatal(SPProgramID.toProgramID(pidStr))
                    .leftMap(_ => badRequest(pidStr, "program"))
    checkId.as { fetchDecodeAndStore[Prog](pidStr, Importer.importProgram) }.merge
  }
}

object ImportServer extends ServerApp {
  val Log = Logger.getLogger(ImportServer.getClass.getName)

  // Port where our http service will run.
  val port              = 8989

  // How long to wait for the import to complete before giving up.
  val timeout: Duration = 30 seconds

  private def fetchServiceUrl(hostName: String): String =
    s"http://$hostName:8442/ocs3/fetch"

  type Obs  = Observation[Step[InstrumentConfig]]
  type Prog = Program[Obs]

  case class ServerResponse(status: Status, msg: String) {

    // Convert to Task[Response] as required by http4s.
    def send: Task[Response] = {
      val enc = EntityEncoder[String]
      var h = enc.headers
      enc.toEntity(msg).flatMap { case EntityEncoder.Entity(proc, len) =>
        len.foreach { l => h = h.put(`Content-Length`(l)) }
        Task.now(Response(status = status, headers = h, body = proc))
      }
    }
  }

  implicit class PioErrorSyntax(e: PioError) {
    def toResponse(id: String): ServerResponse = {
      val msg = e match {
        case MissingKey(name)            => s"missing '$name'"
        case NullValue(name)             => s"null value '$name'"
        case ParseError(value, dataType) => s"could not parse '$value' as '$dataType'"
        case GeneralError(s)             => s"general error $s"
      }
      ServerResponse(InternalServerError, s"Error parsing $id: $msg")
    }
  }

  override def server(args: List[String]): Task[Server] = {
    val hostName = args match {
      case Nil       => "localhost"
      case host :: _ => host
    }

    Log.info(s"Starting import server on port $port connecting to ${fetchServiceUrl(hostName)}")

    val importServer = new ImportServer(hostName)

    val service = HttpService {
      case GET -> Root / "obs" / obsId =>
        importServer.importObservation(obsId).send

      case GET -> Root / "prog" / progId =>
        importServer.importProgram(progId).send
    }

    BlazeBuilder
        .bindHttp(8989, "localhost")
        .mountService(service, "/import")
        .start
  }
}