package edu.gemini.seqexec.web.server.http4s

import org.http4s.server.Server
import org.http4s.server.blaze.BlazeBuilder

object WebServerLauncher extends App {
  def launch(port: Int):Option[Server] = {
    try {
      Some(BlazeBuilder.bindHttp(port)
        .mountService(StaticRoutes.service, "/")
        .mountService(RestRoutes.service, "/api")
        .run)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        None
    }
  }
  launch(9090).foreach(_.awaitShutdown())
}
