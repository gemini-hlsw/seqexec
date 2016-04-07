package edu.gemini.seqexec.web.server.play

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.{ExecutorImpl, SeqexecFailure}
import edu.gemini.seqexec.web.common.{Sequence, SequenceState}
import edu.gemini.seqexec.web.server.model.CannedModel
import play.api.mvc.{Action, Results}
import play.api.routing.Router._
import play.api.routing.sird._
import upickle.default._
import edu.gemini.seqexec.web.server.model.Conversions._

import scalaz.{-\/, \/-}

/**
  * Routes for calls from the web ui
  */
object SeqexecUIApiRoutes {
  val routes: Routes = {
    case GET(p"/api/seqexec/sequence/$id<.*-[0-9]+>") => Action {
      val obsId = new SPObservationID(id)
      ExecutorImpl.read(obsId) match {
        case \/-(s) => Results.Ok(write(List(Sequence(obsId.stringValue(), SequenceState.NotRunning, "Flamingos2", s.toSequenceSteps, None))))
        case -\/(e) => Results.NotFound(SeqexecFailure.explain(e))
      }
    }
    case GET(p"/api/seqexec/current/queue") => Action {
      Results.Ok(write(CannedModel.currentQueue))
    }
  }
}
