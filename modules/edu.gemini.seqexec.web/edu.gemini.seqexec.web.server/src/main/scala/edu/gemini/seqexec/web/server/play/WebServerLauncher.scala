package edu.gemini.seqexec.web.server.play

import edu.gemini.seqexec.web.server.http4s.WebServerLauncher._
import play.api.routing.Router
import play.api.{BuiltInComponents, Environment, Mode}
import play.core.server.{NettyServerComponents, ServerConfig}
import play.api.mvc._

object WebServerLauncher extends App {
  val devMode = !args.contains("prod")

  def launch(port: Int):NettyServerComponents = {
    new NettyServerComponents with BuiltInComponents with Controller {
      override lazy val serverConfig = ServerConfig(
        port = Some(port),
        address = "0.0.0.0"
      ).copy(mode = if (devMode) Mode.Dev else Mode.Prod)

      lazy val router = Router.from(
        SeqexecUIApiRoutes.routes.orElse(
        SeqexecCommandApiRoutes.routes).orElse(
        new StaticAssetsRoutes(environment).routes))
  }}

  launch(9090).server

}
