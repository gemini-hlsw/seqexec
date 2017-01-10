package gem.seqimporter

import gem.{Observation, Step}
import gem.config.InstrumentConfig

import org.http4s.HttpService
import org.http4s.dsl._
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze.BlazeBuilder

import java.util.logging.{Level, Logger}

import scala.concurrent.duration._
import scala.language.postfixOps

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

/** A server that accepts HTTP requests to import a sequence for a particular
  * observation id.  This could be the target of the OT "Queue" button, for
  * example.
  */
object SequenceServer extends ServerApp {
  val Log = Logger.getLogger(SequenceServer.getClass.getName)

  private def fetchSequence(obsIdStr: String): Throwable \/ List[Step[(InstrumentConfig, Boolean)]] =
    for {
      oid <- Observation.Id.fromString(obsIdStr) \/> new RuntimeException(s"Could not parse '$obsIdStr' as an observation id")
      seq <- SequenceImporter.fetchSequence(oid).unsafePerformSyncAttemptFor(30 seconds)
    } yield seq

  val service = HttpService {
    case GET -> Root / obsId =>
      // TODO: all errors are mapped to "Not Found" which is a bit dubious.
      fetchSequence(obsId) match {
        case -\/(t) =>
          Log.log(Level.WARNING, s"Could not import '$obsId'", t)
          NotFound(s"Sorry, could not import '$obsId'.")

        case \/-(s) =>
          Ok(s.mkString("\n"))
      }
  }

  override def server(args: List[String]): Task[Server] = {
    BlazeBuilder
        .bindHttp(8989, "localhost")
        .mountService(service, "/seq")
        .start
  }
}
