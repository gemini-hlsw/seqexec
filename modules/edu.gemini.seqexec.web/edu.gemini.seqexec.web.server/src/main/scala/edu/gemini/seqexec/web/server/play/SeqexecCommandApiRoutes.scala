package edu.gemini.seqexec.web.server.play

import edu.gemini.seqexec.server.osgi.Commands
import play.api.mvc.{Action, Results}
import play.api.routing.Router.Routes
import play.api.routing.sird._

object SeqexecCommandApiRoutes {
  val routes: Routes = {
    case GET(p"/api/seqexec/commands/host") => Action {
      Results.Ok(Commands().seq("host", Array.empty[String]))
    }
  }

}
