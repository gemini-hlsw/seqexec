package edu.gemini.seqexec.web.server.play

import edu.gemini.seqexec.server.{CommandError, CommandResponse, Commands}
import edu.gemini.seqexec.web.common.StepConfig
import play.api.mvc.{Action, Results}
import play.api.routing.Router.Routes
import play.api.routing.sird._
import upickle.default._

import scalaz.\/

case class CommandResult(command: String, error: Boolean, response: String)
case class SequenceConfig(command: String, error: Boolean, response: String, keys: List[StepConfig])
case class SequenceStatus(command: String, error: Boolean, response: String, steps: List[String])

/**
  * Define routes for command-line utilities that want to talk to the seqexec server
  */
object SeqexecCommandApiRoutes {
  val commands = Commands()

  def toCommandResult(s: String, r: CommandError \/ CommandResponse): CommandResult = {
    r.fold(
      l => CommandResult(s, error = true, l.msg),
      m => CommandResult(s, error = false, m.msg)
    )
  }

  def toSequenceConfig(s: String, r: CommandError \/ CommandResponse): SequenceConfig = {
    r.fold(
      l => SequenceConfig(s, error = true, l.msg, Nil),
      m => SequenceConfig(s, error = false, m.msg, m.keys.map(Function.tupled(StepConfig.apply)))
    )
  }

  def toSequenceStatus(s: String, r: CommandError \/ CommandResponse): SequenceStatus = {
    r.fold(
      l => SequenceStatus(s, error = true, l.msg, Nil),
      m => SequenceStatus(s, error = false, m.msg, m.steps)
    )
  }

  val routes: Routes = {
    // Get Seqexec host
    case GET(p"/api/seqexec/commands/host") => Action {
      Results.Ok(write(toCommandResult("host", commands.host())))
    }
    // Set Seqexec host passing parameters as form params
    case POST(p"/api/seqexec/commands/host") => Action { request =>
      request.body.asFormUrlEncoded.filter(_.contains("host")).map { p =>
        val h = p.get("host").flatMap(_.headOption).getOrElse("")
        Results.Ok(write(toCommandResult(s"host $h", commands.host(h))))
      }.getOrElse(Results.BadRequest)
    }
    // Get obs step count
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/count") => Action { request =>
      Results.Ok(write(toCommandResult("show", commands.showCount(obsId))))
    }
    // Get obs static description
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/static") => Action { request =>
      Results.Ok(write(toSequenceConfig("show", commands.showStatic(obsId))))
    }
    // Get obs static description for a given system
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/static/$component") => Action { request =>
      Results.Ok(write(toSequenceConfig("show", commands.showStatic(obsId, component))))
    }
    // Get obs dynamic config for step
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/dynamic/$step") => Action { request =>
      Results.Ok(write(toSequenceConfig("show", commands.showDynamic(obsId, step))))
    }
    // Get obs static description for a given system
    case GET(p"/api/seqexec/commands/$obsId<.*-[0-9]+>/dynamic/$step/$component") => Action { request =>
      Results.Ok(write(toSequenceConfig("show", commands.showDynamic(obsId, step, component))))
    }
    // Runs a particular sequence
    case POST(p"/api/seqexec/commands/$obsId/run") => Action { request =>
      Results.Ok(write(toCommandResult("run", commands.run(obsId))))
    }
    // Stops a particular sequence
    case POST(p"/api/seqexec/commands/$obsId/stop") => Action { request =>
      Results.Ok(write(toCommandResult("stop", commands.stop(obsId))))
    }
    // Continues a particular sequence
    case POST(p"/api/seqexec/commands/$obsId/continue") => Action { request =>
      // Not implemented at lower levels
      Results.Ok(write(toSequenceConfig("continue", commands.continue(obsId))))
    }
    // Gets the status of a sequence
    case GET(p"/api/seqexec/commands/$obsId/state") => Action { request =>
      // Not implemented at lower levels
      Results.Ok(write(toSequenceStatus("status", commands.state(obsId))))
    }

  }

}
