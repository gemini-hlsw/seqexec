package edu.gemini.seqexec.web.server.http4s

import java.time.Instant
import java.util.logging.Logger

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine
import edu.gemini.seqexec.model.Model.{SeqexecEvent, SequenceId, SequencesQueue}
import edu.gemini.seqexec.model.Model.SeqexecEvent.ConnectionOpenEvent
import edu.gemini.seqexec.model._
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.web.common._
import edu.gemini.seqexec.web.server.security.AuthenticationService
import edu.gemini.seqexec.web.server.http4s.encoder._
import edu.gemini.spModel.core.SPBadIDException
import org.http4s._
import org.http4s.server.syntax._
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
class SeqexecUIApiRoutes(auth: AuthenticationService, events: (engine.EventQueue, Topic[SeqexecEvent]), se: SeqexecEngine) extends BooEncoders with ModelBooPicklers {

  // Logger for client messages
  val clientLog = Logger.getLogger("clients")

  /**
    * Creates a process that sends a ping every second to keep the connection alive
    */
  def pingProcess = {
    import scalaz.stream.DefaultScheduler
    import scalaz.stream.time.awakeEvery
    import scalaz.concurrent.Strategy
    import scala.concurrent.duration._

    awakeEvery(1.seconds)(Strategy.DefaultStrategy, DefaultScheduler).map { _ => Ping() }
  }

  val tokenAuthService = JwtAuthentication(auth)

  val publicService: HttpService = GZip { HttpService {

    case req @ POST -> Root / "seqexec" / "login" =>
      req.decode[UserLoginRequest] { (u: UserLoginRequest) =>
        // Try to authenticate
        auth.authenticateUser(u.username, u.password) match {
          case \/-(user) =>
            // if successful set a cookie
            val cookieVal = auth.buildToken(user)
            val expiration = Instant.now().plusSeconds(auth.sessionTimeout.toSeconds.toLong)
            val cookie = Cookie(auth.config.cookieName, cookieVal,
              path = "/".some, expires = expiration.some, secure = auth.config.useSSL, httpOnly = true)
            Ok(user).addCookie(cookie)
          case -\/(_) =>
            Unauthorized(Challenge("jwt", "seqexec"))
        }
      }
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

  def userInRequest(req: Request) = req.attributes.get(JwtAuthentication.authenticatedUser).flatten

  val (inputQueue, engineOutput) = events
  val protectedServices: HttpService =
    tokenAuthService {
      GZip {
        HttpService {
          case req @ GET -> Root / "seqexec" / "events"         =>
            // Stream seqexec events to clients and a ping
            val user = userInRequest(req)
            WS(
              Exchange(
                Process.emit(Binary(trimmedArray(ConnectionOpenEvent(user)))) ++
                  (pingProcess merge engineOutput.subscribe.map(v => Binary(trimmedArray(v)))),
                scalaz.stream.Process.empty
              )
            )

          case req @ POST -> Root / "seqexec" / "logout"              =>
            val user = userInRequest(req)
            user.fold(Unauthorized(Challenge("jwt", "seqexec"))) { _ =>
              // Clean the auth cookie
              val cookie = Cookie(auth.config.cookieName, "", path = "/".some,
                secure = auth.config.useSSL, maxAge = Some(-1), httpOnly = true)
              Ok("").removeCookie(cookie)
            }

          case req @ GET -> Root / "seqexec" / "sequence" / oid =>
            val user = userInRequest(req)
            user.fold(Unauthorized(Challenge("jwt", "seqexec"))) { _ =>
              for {
                obsId <-
                    \/.fromTryCatchNonFatal(new SPObservationID(oid))
                      .fold(Task.fail, Task.now)
                u     <- se.load(inputQueue, obsId)
                resp  <- u.fold(_ => NotFound(s"Not found sequence $oid"), _ => Ok(SequencesQueue[SequenceId](List(oid))))
              } yield resp
            }.handleWith {
              case e: SPBadIDException => BadRequest(s"Bad sequence id $oid")
            }

        }
      }
    }

  def service = publicService || protectedServices || logService
}
