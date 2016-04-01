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
  val commands = Commands()

  val routes: Routes = {
    // Get Seqexec host
    case GET(p"/api/seqexec/commands/host") => Action {
      Results.Ok(write(CommandResponse("host", commands.seq("host", Array.empty[String]))))
    }
    // Set Seqexec host passing parameters as form params
    case POST(p"/api/seqexec/commands/host") => Action { request =>
      request.body.asFormUrlEncoded.filter(_.contains("host")).map { p =>
        val h = p.get("host").flatMap(_.headOption).getOrElse("")
        Results.Ok(write(CommandResponse(s"host $h", commands.seq("host", Array(h)))))
      }.getOrElse(Results.BadRequest)
    }
    // Get obs id count
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/count") => Action { request =>
      Results.Ok(write(CommandResponse("show", commands.seq("show", Array(obsId, "count")))))
    }
  }

}
