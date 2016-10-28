package edu.gemini.seqexec.web.server.http4s

import edu.gemini.seqexec.server.Commands
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.engine
import edu.gemini.seqexec.web.server.model.CommandsModel._
import edu.gemini.seqexec.web.server.http4s.encoder._
import edu.gemini.seqexec.web.server.security.AuthenticationService
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.middleware.GZip

/**
  * Rest Endpoints under the /api route
  */
class SeqexecCommandRoutes(auth: AuthenticationService, q: engine.EventQueue) extends BooPicklers {

  val tokenAuthService = JwtAuthentication(auth)

  val commands = Commands()

  val service = tokenAuthService { GZip { HttpService {
    case req @ GET  -> Root  / "host" =>
      Ok(toCommandResult("host", commands.host()))

    case req @ POST -> Root  / "host" =>
      req.decode[UrlForm] { data =>
        data.getFirst("host") match {
          case Some(h) => Ok(toCommandResult(s"host $h", commands.host(h)))
          case _ => BadRequest("Missing param host")
        }
      }.handleWith {
        case e: Exception => BadRequest("Bad host request")
      }

    case req @ GET  -> Root  / obsId / "count" =>
      Ok(toCommandResult("count", commands.showCount(obsId)))

    case req @ GET  -> Root  / obsId / "static" =>
      Ok(toSequenceConfig("show", commands.showStatic(obsId)))

    case req @ GET  -> Root  / obsId / "static" / system =>
      Ok(toSequenceConfig("show", commands.showStatic(obsId, system)))

    case req @ GET  -> Root  / obsId / "dynamic" / step =>
      Ok(toSequenceConfig("show", commands.showDynamic(obsId, step)))

    case req @ GET  -> Root  / obsId / "dynamic" / step / system =>
      Ok(toSequenceConfig("show", commands.showDynamic(obsId, step, system)))

    case req @ POST -> Root  / obsId / "run" =>
      Ok(toCommandResult("run", commands.run(obsId)))

    case req @ POST -> Root  / obsId / "stop" =>
      Ok(toCommandResult("stop", commands.stop(obsId)))

    case req @ POST -> Root  / obsId / "continue" =>
      Ok(toCommandResult("continue", commands.continue(obsId)))

    case req @ GET  -> Root  / obsId / "state" =>
      Ok(toSequenceConfig("state", commands.state(obsId)))

    // New SeqexecEngine

    // TODO: Add obsId parameter
    case req @ POST -> Root / "start" =>
      // TODO: Get rid of `.toString` How do we want to represent input results
      // now?
      Ok(SeqexecEngine.start(q).map(_.toString))

    // TODO: Add obsId parameter
    case req @ POST -> Root / "pause" =>
      // TODO: Get rid of `.toString` How do we want to represent input results
      // now?
      Ok(SeqexecEngine.requestPause(q).map(_.toString))

    case req @ GET -> Root / "refresh" =>
      // TODO: Get rid of `.toString` How do we want to represent input results
      // now?
      Ok(SeqexecEngine.requestRefresh(q).map(_.toString))

  }}}
}
