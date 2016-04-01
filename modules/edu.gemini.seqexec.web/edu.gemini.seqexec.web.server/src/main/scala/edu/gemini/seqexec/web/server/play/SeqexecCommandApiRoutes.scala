package edu.gemini.seqexec.web.server.play

import edu.gemini.seqexec.server.osgi.Commands
import play.api.mvc.{Action, Results}
import play.api.routing.Router.Routes
import play.api.routing.sird._

import upickle.default._

case class CommandResponse(command: String, error: Boolean, response: String)

import scalaz._
import Scalaz._

/**
  * Define routes for command-line utilities that want to talk to the seqexec server
  */
object SeqexecCommandApiRoutes {
  val commands = Commands()

  def toCommandResponse(s: String, r: Commands.CommandError \/ String): CommandResponse = {
    r.fold(
      l => CommandResponse(s, error = true, l.msg),
      r => CommandResponse(s, error = false, r)
    )
  }

  val routes: Routes = {
    // Get Seqexec host
    case GET(p"/api/seqexec/commands/host") => Action {
      Results.Ok(write(toCommandResponse("host", commands.seq("host", Nil))))
    }
    // Set Seqexec host passing parameters as form params
    case POST(p"/api/seqexec/commands/host") => Action { request =>
      request.body.asFormUrlEncoded.filter(_.contains("host")).map { p =>
        val h = p.get("host").flatMap(_.headOption).getOrElse("")
        Results.Ok(write(toCommandResponse(s"host $h", commands.seq("host", List(h)))))
      }.getOrElse(Results.BadRequest)
    }
    // Get obs step count
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/count") => Action { request =>
      Results.Ok(write(toCommandResponse("show", commands.seq(s"show", List(obsId, "count")))))
    }
    // Get obs static description
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/static") => Action { request =>
      Results.Ok(write(toCommandResponse("show", commands.seq(s"show", List(obsId, "static")))))
    }
  }

}
