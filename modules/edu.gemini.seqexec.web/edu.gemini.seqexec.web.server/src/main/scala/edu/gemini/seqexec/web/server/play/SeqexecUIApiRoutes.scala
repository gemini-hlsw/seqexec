package edu.gemini.seqexec.web.server.play

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.{ExecutorImpl, SeqexecFailure}
import edu.gemini.seqexec.web.common.{Sequence, SequenceState, UserLoginRequest}
import edu.gemini.seqexec.web.server.model.CannedModel
import play.api.mvc.{Action, BodyParsers, Results}
import play.api.routing.Router._
import play.api.routing.sird._
import upickle.default._
import edu.gemini.seqexec.web.server.model.Conversions._
import edu.gemini.seqexec.web.server.security.{LDAPService, TestAuthenticationService}

import scalaz.{-\/, \/-}

/**
  * Routes for calls from the web ui
  */
object SeqexecUIApiRoutes {
  // TODO Pass the configuration as a param
  val ldapService = new LDAPService("gs-dc6.gemini.edu", 3268)

  // TODO Only the LDAP service should be present on production mode
  val authServices = List(TestAuthenticationService, ldapService)

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
    case POST(p"/api/seqexec/login") => Action(BodyParsers.parse.text) { s =>
      val u = read[UserLoginRequest](s.body)
      authServices.authenticateUser(u.username, u.password) match {
        case \/-(user) => Results.Ok(write(user))
        case -\/(_)    => Results.Unauthorized("")
      }
    }
  }
}
