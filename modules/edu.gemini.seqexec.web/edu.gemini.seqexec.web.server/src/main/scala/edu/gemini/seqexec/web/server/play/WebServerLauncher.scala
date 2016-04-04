package edu.gemini.seqexec.web.server.play

import play.api.routing.Router
import play.api.{BuiltInComponents, Mode}
import play.core.server.{NettyServerComponents, ServerConfig}
import play.api.mvc._

object WebServerLauncher extends App {

  def launch(port: Int):NettyServerComponents = {
    new NettyServerComponents with BuiltInComponents with Controller {
      override lazy val serverConfig = ServerConfig(
        port = Some(port),
        address = "0.0.0.0"
      ).copy(mode = Mode.Dev)

      lazy val router = Router.from(
        SeqexecUIApiRoutes.routes.orElse(
        SeqexecCommandApiRoutes.routes).orElse(
        StaticAssetsRoutes.routes))
  }}

  launch(9090).server

}
