package edu.gemini.seqexec.web.server.play

import edu.gemini.seqexec.server.Commands
import edu.gemini.seqexec.web.server.model.CommandsModel._
import play.api.http.ContentTypes
import play.api.mvc.{Action, Results}
import play.api.routing.Router.Routes
import play.api.routing.sird._
import boopickle.Default._

/**
  * Define routes for command-line utilities that want to talk to the seqexec server
  */
object SeqexecCommandApiRoutes {
  val commands = Commands()

  val routes: Routes = {
    // Get Seqexec host
    case GET(p"/api/seqexec/commands/host") => Action {
      Results.Ok(Pickle.intoBytes(toCommandResult("host", commands.host())).array()).as(ContentTypes.BINARY)
    }
    // Set Seqexec host passing parameters as form params
    case POST(p"/api/seqexec/commands/host") => Action { request =>
      request.body.asFormUrlEncoded.filter(_.contains("host")).map { p =>
        val h = p.get("host").flatMap(_.headOption).getOrElse("")
        Results.Ok(Pickle.intoBytes(toCommandResult(s"host $h", commands.host(h))).array()).as(ContentTypes.BINARY)
      }.getOrElse(Results.BadRequest)
    }
    // Get obs step count
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/count") => Action { request =>
      Results.Ok(Pickle.intoBytes(toCommandResult("show", commands.showCount(obsId))).array()).as(ContentTypes.BINARY)
    }
    // Get obs static description
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/static") => Action { request =>
      Results.Ok(Pickle.intoBytes(toSequenceConfig("show", commands.showStatic(obsId))).array()).as(ContentTypes.BINARY)
    }
    // Get obs static description for a given system
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/static/$component") => Action { request =>
      Results.Ok(Pickle.intoBytes(toSequenceConfig("show", commands.showStatic(obsId, component))).array()).as(ContentTypes.BINARY)
    }
    // Get obs dynamic config for step
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/dynamic/$step") => Action { request =>
      Results.Ok(Pickle.intoBytes(toSequenceConfig("show", commands.showDynamic(obsId, step))).array()).as(ContentTypes.BINARY)
    }
    // Get obs static description for a given system
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/dynamic/$step/$component") => Action { request =>
      Results.Ok(Pickle.intoBytes(toSequenceConfig("show", commands.showDynamic(obsId, step, component))).array()).as(ContentTypes.BINARY)
    }
    // Runs a particular sequence
    case POST(p"/api/seqexec/commands/$obsId/run") => Action { request =>
      Results.Ok(Pickle.intoBytes(toCommandResult("run", commands.run(obsId))).array()).as(ContentTypes.BINARY)
    }
    // Stops a particular sequence
    case POST(p"/api/seqexec/commands/$obsId/stop") => Action { request =>
      Results.Ok(Pickle.intoBytes(toCommandResult("stop", commands.stop(obsId))).array()).as(ContentTypes.BINARY)
    }
    // Continues a particular sequence
    case POST(p"/api/seqexec/commands/$obsId/continue") => Action { request =>
      // Not implemented at lower levels
      Results.Ok(Pickle.intoBytes(toSequenceConfig("continue", commands.continue(obsId))).array()).as(ContentTypes.BINARY)
    }
    // Gets the status of a sequence
    case GET(p"/api/seqexec/commands/$obsId/state") => Action { request =>
      // Not implemented at lower levels
      Results.Ok(Pickle.intoBytes(toSequenceStatus("state", commands.state(obsId))).array()).as(ContentTypes.BINARY)
    }

  }

}
