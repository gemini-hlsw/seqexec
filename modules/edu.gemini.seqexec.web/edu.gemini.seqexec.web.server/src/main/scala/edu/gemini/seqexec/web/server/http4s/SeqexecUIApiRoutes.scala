package edu.gemini.seqexec.web.server.http4s

import java.time.Instant
import java.util.logging.Logger

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.SeqexecFailure.Unexpected
import edu.gemini.seqexec.server.{ExecutorImpl, SeqexecFailure}
import edu.gemini.seqexec.web.common._
import edu.gemini.seqexec.web.common.LogMessage._
import edu.gemini.seqexec.web.server.model.CannedModel
import edu.gemini.seqexec.web.server.security.AuthenticationService._
import edu.gemini.seqexec.web.server.model.Conversions._
import edu.gemini.seqexec.web.server.security.AuthenticationConfig
import org.http4s._
import org.http4s.server.syntax._
import org.http4s.dsl._
import upickle.default._
import org.http4s.server.websocket._
import org.http4s.websocket.WebsocketBits._

import scalaz._
import Scalaz._
import scalaz.stream.Exchange

/**
  * Rest Endpoints under the /api route
  */
object SeqexecUIApiRoutes {
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

  val tokenAuthService = new JwtAuthentication

  val publicService: HttpService = HttpService {
    case req @ GET -> Root  / "seqexec" / "current" / "queue" =>
      Ok(write(CannedModel.currentQueue))
    case req @ POST -> Root  / "seqexec" / "login" =>
      req.decode[String] { body =>
        val u = read[UserLoginRequest](body)
        // Try to authenticate
        AuthenticationConfig.authServices.authenticateUser(u.username, u.password) match {
          case \/-(user) =>
            // if successful set a cookie
            val cookieVal = buildToken(user)
            val expiration = Instant.now().plusSeconds(AuthenticationConfig.sessionTimeout)
            val cookie = Cookie(AuthenticationConfig.cookieName, cookieVal, path = "/".some, expires = expiration.some, secure = AuthenticationConfig.onSSL, httpOnly = true)
            Ok(write(user)).addCookie(cookie)
          case -\/(_)    =>
            Unauthorized(Challenge("jwt", "seqexec"))
        }
      }
    case req @ GET -> Root  / "seqexec" / "sequence" / oid =>
      val r = for {
        obsId <- \/.fromTryCatchNonFatal(new SPObservationID(oid)).leftMap((t:Throwable) => Unexpected(t.getMessage))
        s     <- ExecutorImpl.read(obsId)
      } yield (obsId, s)

      r match {
        case \/-((i, s)) => Ok(write(List(Sequence(i.stringValue(), SequenceState.NotRunning, "F2", s.toSequenceSteps, None))))
        case -\/(e)      => NotFound(SeqexecFailure.explain(e))
      }
    case GET -> Root / "seqexec" / "events" =>
      // Stream seqexec events to clients and a ping
      WS(Exchange(pingProcess merge ExecutorImpl.sequenceEvents.map(v => Text(write(v))), scalaz.stream.Process.empty))
    case req @ POST -> Root / "seqexec" / "log" =>
      req.decode[String] { body =>
        val u = read[LogMessage](body)
        // This will use the server time for the logs
        clientLog.log(u.level, s"Client ${req.remoteAddr}: ${u.msg}")
        // Always return ok
        Ok()
      }
  }

  val protectedServices: HttpService = tokenAuthService { HttpService {
      case req @ POST -> Root / "seqexec" / "logout" =>
        // This is not necessary, it is just code to verify token decoding
        println("Logged out " + req.attributes.get(JwtAuthentication.authenticatedUser))

        val cookie = Cookie(AuthenticationConfig.cookieName, "", path = "/".some, secure = AuthenticationConfig.onSSL, maxAge = Some(-1), httpOnly = true)
        Ok("").removeCookie(cookie)
    }
  }

  val service = publicService || protectedServices
}
