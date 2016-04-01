package edu.gemini.seqexec.web.server.play

import edu.gemini.seqexec.server.osgi.Commands
import play.api.mvc.{Action, Results}
import play.api.routing.Router.Routes
import play.api.routing.sird._

import upickle.default._

case class CommandResponse(command: String, response: String)

/**
  * Define routes for command-line utilities that want to talk to the seqexec server
  */
object SeqexecCommandApiRoutes {
  val routes: Routes = {
    case GET(p"/api/seqexec/commands/host") => Action {
      Results.Ok(write(CommandResponse("host", Commands().seq("host", Array.empty[String]))))
    }
  }

}
