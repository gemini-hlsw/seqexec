package edu.gemini.seqexec.web.server.play

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Source, _}
import akka.util.ByteString
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.{ExecutorImpl, SeqexecFailure}
import edu.gemini.seqexec.web.common.{Sequence, SequenceState, UserLoginRequest}
import edu.gemini.seqexec.web.server.model.CannedModel
import play.api.mvc.{Action, Results, WebSocket}
import play.api.routing.Router._
import play.api.routing.sird._
import upickle.default._
import edu.gemini.seqexec.web.server.model.Conversions._
import play.api.http.websocket.{Message, PingMessage, TextMessage}
import play.api.mvc.WebSocket.MessageFlowTransformer
import edu.gemini.seqexec.web.server.security.AuthenticationService._
import edu.gemini.seqexec.web.server.security.AuthenticationConfig

import scalaz.{-\/, \/-}
import streamz.akka.stream._

/**
  * Routes for calls from the web ui
  */
object SeqexecUIApiRoutes {
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
      // Merge the ping and events from ExecutorImpl
      val events = pingProcess merge ExecutorImpl.sequenceEvents.map(v => TextMessage(write(v)))

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
        case -\/(_)    =>
          Results.Unauthorized("")
      }
    }
  }
}
