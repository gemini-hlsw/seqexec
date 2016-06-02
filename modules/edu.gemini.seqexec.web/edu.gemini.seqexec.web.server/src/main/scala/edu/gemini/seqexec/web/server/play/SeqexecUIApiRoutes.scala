package edu.gemini.seqexec.web.server.play

import java.util.logging.Logger

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.model.{SeqexecConnectionOpenEvent, UserLoginRequest}
import edu.gemini.seqexec.server.{ExecutorImpl, SeqexecFailure}
import edu.gemini.seqexec.web.common.{LogMessage, Sequence, SequenceState, UserLoginRequest}
import edu.gemini.seqexec.web.common.LogMessage._
import edu.gemini.seqexec.web.server.model.CannedModel
import edu.gemini.seqexec.web.server.model.Conversions._
import edu.gemini.seqexec.web.server.security.AuthenticationService._
import edu.gemini.seqexec.web.server.security.AuthenticationConfig

import play.api.mvc._
import play.api.routing.Router._
import play.api.routing.sird._
import play.api.http.websocket.{Message, PingMessage, TextMessage}
import play.api.mvc.WebSocket.MessageFlowTransformer
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Source, _}
import akka.util.ByteString

import upickle.default._

import scalaz.{-\/, \/-}
import streamz.akka.stream._

import scalaz.stream.Process

/**
  * Routes for calls from the web ui
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

    awakeEvery(1.seconds)(Strategy.DefaultStrategy, DefaultScheduler).map{ d => PingMessage(ByteString.empty) }
  }

  implicit val system = ActorSystem("seqexec")

  implicit val messageFlowTransformer = MessageFlowTransformer.identityMessageFlowTransformer

  val routes: Routes = {
    case GET(p"/api/seqexec/sequence/$id<.*-[0-9]+>") => Action {
      val obsId = new SPObservationID(id)
      ExecutorImpl.read(obsId) match {
        case \/-(s) => Results.Ok(write(List(Sequence(obsId.stringValue(), SequenceState.NotRunning, "F2", s.toSequenceSteps, None))))
        case -\/(e) => Results.NotFound(SeqexecFailure.explain(e))
      }
    }
    case GET(p"/api/seqexec/current/queue") => Action {
      Results.Ok(write(CannedModel.currentQueue))
    }
    case GET(p"/api/seqexec/events") => WebSocket.accept[Message, Message] { h =>
      val user = UserAction.checkAuth(h).fold(_ => None, Some.apply)

      // Merge the ping and events from ExecutorImpl
      val events = pingProcess merge (Process.emit(TextMessage(write(SeqexecConnectionOpenEvent(user)))) ++ ExecutorImpl.sequenceEvents.map(v => TextMessage(write(v))))

      // Make an akka publisher out of the scalaz stream
      val (p2, publisher) = events.publisher()
      val source = Source.fromPublisher(publisher)

      // We don't really need to listen for completion, so ignore the callback
      p2.run.unsafePerformAsync(x => ())

      // Return a flow ignoring the input stream
      Flow.fromSinkAndSource(Sink.ignore, source)
    }
    case POST(p"/api/seqexec/logout") => UserAction { a =>
      // This is not necessary, it is just code to verify token decoding
      println("Logged out " + a.user)
      Results.Ok("").discardingCookies(DiscardingCookie(AuthenticationConfig.cookieName))
    }
    case POST(p"/api/seqexec/login") => Action(BodyParsers.parse.text) { s =>
      val u = read[UserLoginRequest](s.body)
      // Try to authenticate
      AuthenticationConfig.authServices.authenticateUser(u.username, u.password) match {
        case \/-(user) =>
          // if successful set a cookie
          val cookieVal = buildToken(user)
          val cookie = Cookie(AuthenticationConfig.cookieName, cookieVal, maxAge = Option(AuthenticationConfig.sessionTimeout), secure = AuthenticationConfig.onSSL, httpOnly = true)
          Results.Ok(write(user)).withCookies(cookie)
        case -\/(_) =>
          Results.Unauthorized("")
      }
    }
    case POST(p"/api/seqexec/log") => Action(BodyParsers.parse.text) { s =>
      val u = read[LogMessage](s.body)
      // This will use the server time for the logs
      clientLog.log(u.level, s"Client ${s.remoteAddress}: ${u.msg}")
      // Always return ok
      Results.Ok("")
    }
  }
}
