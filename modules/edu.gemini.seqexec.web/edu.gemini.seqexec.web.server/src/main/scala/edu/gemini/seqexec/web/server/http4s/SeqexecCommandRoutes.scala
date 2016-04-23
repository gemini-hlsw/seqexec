package edu.gemini.seqexec.web.server.http4s

import edu.gemini.seqexec.server.Commands
import edu.gemini.seqexec.web.server.model.CommandsModel._
import org.http4s._
import org.http4s.dsl._
import upickle.default._

/**
  * Rest Endpoints under the /api route
  */
object SeqexecCommandRoutes {
  val commands = Commands()

  val service = HttpService {
    case req @ GET  -> Root  / "host" =>
      Ok(write(toCommandResult("host", commands.host())))

    case req @ POST -> Root  / "host" =>
      req.decode[UrlForm] { data =>
        data.getFirst("host") match {
          case Some(h) => Ok(write(toCommandResult(s"host $h", commands.host(h))))
          case _ => BadRequest("Missing param host")
        }
      }.handleWith {
        case e: Exception => BadRequest("Bad host request")
      }

    case req @ GET  -> Root  / obsId / "count" =>
      Ok(write(toCommandResult("count", commands.showCount(obsId))))

    case req @ GET  -> Root  / obsId / "static" =>
      Ok(write(toSequenceConfig("show", commands.showStatic(obsId))))

    case req @ GET  -> Root  / obsId / "static" / system =>
      Ok(write(toSequenceConfig("show", commands.showStatic(obsId, system))))

    case req @ GET  -> Root  / obsId / "dynamic" / step =>
      Ok(write(toSequenceConfig("show", commands.showDynamic(obsId, step))))

    case req @ GET  -> Root  / obsId / "dynamic" / step / system =>
      Ok(write(toSequenceConfig("show", commands.showDynamic(obsId, step, system))))

    case req @ POST -> Root  / obsId / "run" =>
      Ok(write(toCommandResult("run", commands.run(obsId))))

    case req @ POST -> Root  / obsId / "stop" =>
      Ok(write(toCommandResult("stop", commands.stop(obsId))))

    case req @ POST -> Root  / obsId / "continue" =>
      Ok(write(toCommandResult("continue", commands.continue(obsId))))

    case req @ GET  -> Root  / obsId / "state" =>
      Ok(write(toSequenceConfig("state", commands.state(obsId))))
  }
}
