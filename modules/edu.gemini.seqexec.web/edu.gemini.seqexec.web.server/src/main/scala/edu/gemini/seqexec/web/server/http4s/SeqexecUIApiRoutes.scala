package edu.gemini.seqexec.web.server.http4s

import java.time.Instant
import java.util.logging.Logger

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.model._
import edu.gemini.seqexec.server.SeqexecFailure.Unexpected
import edu.gemini.seqexec.server.{ExecutorImpl, SeqexecFailure}
import edu.gemini.seqexec.web.common._
import edu.gemini.seqexec.web.common.picklers._
import edu.gemini.seqexec.web.server.model.CannedModel
import edu.gemini.seqexec.web.server.model.Conversions._
import edu.gemini.seqexec.web.server.security.AuthenticationService._
import edu.gemini.seqexec.web.server.security.AuthenticationConfig
import edu.gemini.seqexec.web.server.http4s.encoder._
import org.http4s._
import org.http4s.server.syntax._
import org.http4s.dsl._
import org.http4s.server.websocket._
import org.http4s.websocket.WebsocketBits._
import org.http4s.server.middleware.GZip
import boopickle.Default._

import scalaz._
import Scalaz._
import scalaz.stream.{Exchange, Process}

trait BooPickleDecoders {
  import edu.gemini.seqexec.web.common.LogMessage._

  // Decoders, Included here instead of the on the object definitions to avoid
  // a circular dependency on http4s
  implicit val userLoginDecoder = booOf[UserLoginRequest]
  implicit val userDetailEncoder = booEncoderOf[UserDetails]
  implicit val logMessageDecoder = booOf[LogMessage]
  implicit val sequenceEncoder = booEncoderOf[Sequence]
  // The next one seems redundant but it won't work without it
  implicit val listSequenceEncoder = booEncoderOf[List[Sequence]]
  implicit val sequexecQueueEncoder = booEncoderOf[SeqexecQueue]
}

/**
  * Rest Endpoints under the /api route
  */
object SeqexecUIApiRoutes extends BooPickleDecoders {
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
      Ok(CannedModel.currentQueue)

    case req @ POST -> Root  / "seqexec" / "login" =>
      req.decode[UserLoginRequest] { (u: UserLoginRequest) =>
        // Try to authenticate
        AuthenticationConfig.authServices.authenticateUser(u.username, u.password) match {
          case \/-(user) =>
            // if successful set a cookie
            val cookieVal = buildToken(user)
            val expiration = Instant.now().plusSeconds(AuthenticationConfig.sessionTimeout)
            val cookie = Cookie(AuthenticationConfig.cookieName, cookieVal, path = "/".some, expires = expiration.some, secure = AuthenticationConfig.onSSL, httpOnly = true)
            Ok(user).addCookie(cookie)
          case -\/(_)    =>
            Unauthorized(Challenge("jwt", "seqexec"))
        }
      }

    case req @ POST -> Root / "seqexec" / "log" =>
      req.decode[LogMessage] { msg =>
        // This will use the server time for the logs
        clientLog.log(msg.level, s"Client ${req.remoteAddr}: ${msg.msg}")
        // Always return ok
        Ok()
      }
  }

  def userInRequest(req: Request) = req.attributes.get(JwtAuthentication.authenticatedUser).flatten

  val protectedServices: HttpService = tokenAuthService { HttpService {
      case req @ GET -> Root / "seqexec" / "events" =>
        // Stream seqexec events to clients and a ping
        val user = userInRequest(req)

        // Important to set the type as SeqexecEvent
        val initialEvent:SeqexecEvent = SeqexecConnectionOpenEvent(user)
        val byteBuffer = Pickle.intoBytes(initialEvent)
        val bytes = new Array[Byte](byteBuffer.limit())
        byteBuffer.get(bytes, 0, byteBuffer.limit)
        WS(Exchange(pingProcess merge (Process.emit(Binary(bytes)) ++ ExecutorImpl.sequenceEvents.map(v => Binary(Pickle.intoBytes(v).array()))), scalaz.stream.Process.empty))

      case req @ POST -> Root / "seqexec" / "logout" =>
        // Clean the auth cookie
        val cookie = Cookie(AuthenticationConfig.cookieName, "", path = "/".some, secure = AuthenticationConfig.onSSL, maxAge = Some(-1), httpOnly = true)
        Ok("").removeCookie(cookie)

      case req @ GET -> Root / "seqexec" / "sequence" / oid =>
        val user = userInRequest(req)
        user.fold(Unauthorized(Challenge("jwt", "seqexec"))) { _ =>
          val r = for {
            obsId <- \/.fromTryCatchNonFatal(new SPObservationID(oid)).leftMap((t:Throwable) => Unexpected(t.getMessage))
            s     <- ExecutorImpl.read(obsId)
          } yield (obsId, s)

          r match {
            case \/-((i, s)) => Ok(List(Sequence(i.stringValue(), SequenceState.NotRunning, "F2", s.toSequenceSteps, None)))
            case -\/(e)      => NotFound(SeqexecFailure.explain(e))
          }
        }
    }

  }

  val service = publicService || protectedServices
}
