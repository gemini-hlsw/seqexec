package edu.gemini.seqexec.web.server.http4s

import org.http4s.server.Server
import org.http4s.server.blaze.BlazeBuilder

object WebServerLauncher extends App {
  val devMode = !args.contains("prod")

  def launch(port: Int):Option[Server] = {
    try {
      Some(BlazeBuilder.bindHttp(port, "0.0.0.0")
        .withWebSockets(true)
        .mountService(StaticRoutes.service(devMode), "/")
        .mountService(SeqexecCommandRoutes.service, "/api/seqexec/commands")
        .mountService(SeqexecUIApiRoutes.service, "/api")
        .run)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        None
    }
  }
  launch(9090).foreach(_.awaitShutdown())
}
