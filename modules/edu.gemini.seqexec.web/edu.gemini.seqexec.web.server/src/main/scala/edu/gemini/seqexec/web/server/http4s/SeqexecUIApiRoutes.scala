package edu.gemini.seqexec.web.server.http4s

import java.time.Instant
import java.util.logging.Logger

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine
import edu.gemini.seqexec.model._
import edu.gemini.seqexec.server.SeqexecFailure.Unexpected
import edu.gemini.seqexec.server.{ODBProxy, SeqexecEngine, SeqexecFailure}
import edu.gemini.seqexec.web.common._
import edu.gemini.seqexec.web.server.model.CannedModel
import edu.gemini.seqexec.web.server.model.Conversions._
import edu.gemini.seqexec.web.server.security.AuthenticationService
import edu.gemini.seqexec.web.server.http4s.encoder._
import org.http4s._
import org.http4s.server.syntax._
import org.http4s.dsl._
import org.http4s.server.websocket._
import org.http4s.websocket.WebsocketBits._
import org.http4s.server.middleware.GZip

import scalaz._
import Scalaz._
import scalaz.stream.{Exchange, Process}

/**
  * Rest Endpoints under the /api route
  */
class SeqexecUIApiRoutes(auth: AuthenticationService, q: engine.EventQueue) extends BooPicklers with NewBooPicklers {

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

    awakeEvery(1.seconds)(Strategy.DefaultStrategy, DefaultScheduler).map { d => Ping() }
  }

  val tokenAuthService = JwtAuthentication(auth)

  val publicService: HttpService = GZip { HttpService {
    case req @ GET -> Root / "seqexec" / "current" / "queue" =>
      Ok(CannedModel.currentQueue)

    case req @ POST -> Root / "seqexec" / "login" =>
      req.decode[UserLoginRequest] { (u: UserLoginRequest) =>
        // Try to authenticate
        auth.authenticateUser(u.username, u.password) match {
          case \/-(user) =>
            // if successful set a cookie
            val cookieVal = auth.buildToken(user)
            val expiration = Instant.now().plusSeconds(auth.sessionTimeout)
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

  val protectedServices: HttpService = tokenAuthService { GZip { HttpService {
      case req @ GET -> Root / "seqexec" / "events"         =>
        // Stream seqexec events to clients and a ping
        val user = userInRequest(req)

        WS(Exchange(pingProcess merge //(Process.emit(Binary(trimmedArray(SeqexecConnectionOpenEvent(user)))) ++
          SeqexecEngine.eventProcess(q).map(v => Binary(newTrimmedArray(v))), scalaz.stream.Process.empty))

      case req @ POST -> Root / "seqexec" / "logout"        =>
        // Clean the auth cookie
        val cookie = Cookie(auth.config.cookieName, "", path = "/".some,
          secure = auth.config.useSSL, maxAge = Some(-1), httpOnly = true)
        Ok("").removeCookie(cookie)

      case req @ GET -> Root / "seqexec" / "sequence" / oid =>
        val user = userInRequest(req)
        user.fold(Unauthorized(Challenge("jwt", "seqexec"))) { _ =>
          val r = for {
            obsId <- \/.fromTryCatchNonFatal(new SPObservationID(oid)).leftMap((t:Throwable) => Unexpected(t.getMessage))
            s     <- ODBProxy.read(obsId)
          } yield (obsId, s)

          r match {
            case \/-((i, s)) => SeqexecEngine.load(q, i) *>
                Ok(List(Sequence(i.stringValue(), SequenceState.NotRunning, "F2", s.toSequenceSteps, None)))
            case -\/(e)      => NotFound(SeqexecFailure.explain(e))
          }
        }
    }}

  }

  def service = publicService || protectedServices || logService
}
