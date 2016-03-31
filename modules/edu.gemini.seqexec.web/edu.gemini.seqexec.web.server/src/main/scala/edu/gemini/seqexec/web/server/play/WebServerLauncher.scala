package edu.gemini.seqexec.web.server.play

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.{ExecutorImpl, SeqexecFailure}
import edu.gemini.seqexec.web.common._
import play.api.routing.Router
import play.api.{BuiltInComponents, Mode}
import play.core.server.{NettyServerComponents, ServerConfig}
import play.api._
import play.api.mvc._
import play.api.routing.sird._
import edu.gemini.seqexec.web.server.model.Conversions._
import upickle.default._

import scalaz._
import Scalaz._


object WebServerLauncher extends App {

  def launch(port: Int):NettyServerComponents = {
    new NettyServerComponents with BuiltInComponents with Controller {
      override lazy val serverConfig = ServerConfig(
        port = Some(port),
        address = "0.0.0.0"
      ).copy(mode = Mode.Dev)

      lazy val router = Router.from {
        case GET(p"/") =>
          // Index
          CustomAssets.at("./src/main/resources", "index.html", "/")
        case GET(p"/api/sequence/$id<.*-[0-9]+>") => Action {
          val obsId = new SPObservationID(id)
          ExecutorImpl.read(obsId) match {
            case \/-(s) => Results.Ok(write(Sequence(obsId.stringValue(), s.toSequenceSteps)))
            case -\/(e) => Results.NotFound(SeqexecFailure.explain(e))
          }
        }
        case GET(p"/$f*") =>
          // Static files,
          CustomAssets.at("src/main/resources", f, "/")
      }
  }}

  launch(9090).server

}
