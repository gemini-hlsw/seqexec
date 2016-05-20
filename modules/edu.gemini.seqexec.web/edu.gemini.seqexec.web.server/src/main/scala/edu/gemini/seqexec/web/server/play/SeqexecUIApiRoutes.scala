package edu.gemini.seqexec.web.server.play

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.{ExecutorImpl, SeqexecFailure}
import edu.gemini.seqexec.web.common.{Sequence, SequenceState, UserLoginRequest}
import edu.gemini.seqexec.web.server.model.CannedModel
import play.api.mvc._
import play.api.routing.Router._
import play.api.routing.sird._
import upickle.default._
import edu.gemini.seqexec.web.server.model.Conversions._
import edu.gemini.seqexec.web.server.security.AuthenticationService._
import edu.gemini.seqexec.web.server.security.AuthenticationConfig

import scalaz.{-\/, \/-}

/**
  * Routes for calls from the web ui
  */
object SeqexecUIApiRoutes {

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
    case POST(p"/api/seqexec/logout") => Action { r =>
      // This is not necessary, it is just code to verify token decoding
      val u = for {
        token   <- r.cookies.get(AuthenticationConfig.cookieName)
        user    <- decodeToken(token.value).toOption
      } yield user
      println("Logged out " + u)

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
