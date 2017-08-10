package edu.gemini.seqexec.web.server.http4s

import java.util.logging.Logger

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server
import edu.gemini.seqexec.model.Model.{Conditions, SeqexecEvent, SequenceId, SequencesQueue}
import edu.gemini.seqexec.model.Model.SeqexecEvent._
import edu.gemini.seqexec.model._
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.web.common._
import edu.gemini.seqexec.web.server.security.{AuthenticationService, Http4sAuthentication, TokenRefresher}
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult
import edu.gemini.seqexec.web.server.http4s.encoder._
import edu.gemini.spModel.core.SPBadIDException

import org.http4s._
import org.http4s.dsl._
import org.http4s.server.websocket._
import org.http4s.websocket.WebsocketBits._
import org.http4s.server.middleware.GZip

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.async.mutable.Topic
import scalaz.stream.{Exchange, Process}

/**
  * Rest Endpoints under the /api route
  */
class SeqexecUIApiRoutes(auth: AuthenticationService, events: (server.EventQueue, Topic[SeqexecEvent]), se: SeqexecEngine) extends BooEncoders with ModelBooPicklers {

  // Logger for client messages
  val clientLog = Logger.getLogger("clients")

  // Handles authentication
  val httpAuthentication = new Http4sAuthentication(auth)

  val (inputQueue, engineOutput) = events

  /**
    * Creates a process that sends a ping every second to keep the connection alive
    */
  private def pingProcess: Process[Task, Ping] = {
    import scalaz.stream.DefaultScheduler
    import scalaz.stream.time.awakeEvery
    import scalaz.concurrent.Strategy
    import scala.concurrent.duration._

    awakeEvery(1.seconds)(Strategy.DefaultStrategy, DefaultScheduler).map { _ => Ping() }
  }

  val publicService: HttpService = GZip { HttpService {

    case req @ POST -> Root / "seqexec" / "login" =>
      req.decode[UserLoginRequest] { (u: UserLoginRequest) =>
        // Try to authenticate
        auth.authenticateUser(u.username, u.password) match {
          case \/-(user) =>
            // if successful set a cookie
            httpAuthentication.loginCookie(user) >>= { cookie => Ok(user).addCookie(cookie) }
          case -\/(_) =>
            Unauthorized(Challenge("jwt", "seqexec"))
        }
      }

      case POST -> Root / "seqexec" / "logout"              =>
        // Clean the auth cookie
        val cookie = Cookie(auth.config.cookieName, "", path = "/".some,
          secure = auth.config.useSSL, maxAge = Some(-1), httpOnly = true)
        Ok("").removeCookie(cookie)

    }}

  // Don't gzip log responses
  val logService: HttpService = HttpService {
    case req @ POST -> Root / "seqexec" / "log" =>
      req.decode[LogMessage] { msg =>
        // This will use the server time for the logs
        clientLog.log(msg.level, s"Client ${req.remoteAddr}: ${msg.msg}")
        // Always return ok
        Ok()
      }
  }
  val protectedServices: AuthedService[AuthResult] =
    AuthedService {
      case GET -> Root / "seqexec" / "events" as user        =>
        // Stream seqexec events to clients and a ping
        def anonymize(e: SeqexecEvent) = {
            // Hide the name for anonymous users
            sequenceNameL.set("")(e)
        }
        def filterOutNull = (e: SeqexecEvent) => e match {
          case NullEvent => false
          case _         => true
        }
        // If the user didn't login, anonymize
        val anonymizeF: SeqexecEvent => SeqexecEvent = user.fold(_ => anonymize _, _ => identity _)
        WS(
          Exchange(
            Process.emit(Binary(trimmedArray(ConnectionOpenEvent(user.toOption)))) ++
              (pingProcess merge engineOutput.subscribe.map(anonymizeF).filter(filterOutNull).map(v => Binary(trimmedArray(v)))),
            scalaz.stream.Process.empty
          )
        )

      case GET -> Root / "seqexec" / "sequence" / oid as user =>
        user.toOption.fold(Unauthorized(Challenge("jwt", "seqexec"))) { _ =>
          for {
            obsId <-
                \/.fromTryCatchNonFatal(new SPObservationID(oid))
                  .fold(Task.fail, Task.now)
            u     <- se.load(inputQueue, obsId)
            resp  <- u.fold(_ => NotFound(s"Not found sequence $oid"), _ =>
              Ok(SequencesQueue[SequenceId](Conditions.default, None, List(oid))))
          } yield resp
        }.handleWith {
          case _: SPBadIDException => BadRequest(s"Bad sequence id $oid")
        }
    }

  def service: Service[Request, MaybeResponse] = publicService |+| TokenRefresher(httpAuthentication, GZip(httpAuthentication.optAuth(protectedServices))) |+| logService
}
