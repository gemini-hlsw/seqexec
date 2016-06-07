package edu.gemini.seqexec.web.server.http4s

import java.util.logging.{LogManager, Logger}

import org.http4s.server.Server
import org.http4s.server.blaze.BlazeBuilder

import scalaz._
import Scalaz._

object WebServerLauncher extends App {
  val logger = Logger.getLogger(getClass.getName)

  // Try to load logging configuration
  \/.fromTryCatchNonFatal(
    LogManager.getLogManager.readConfiguration(getClass.getClassLoader.getResourceAsStream("logging.properties"))
  ).leftMap(_ => println("Cannot load logging configuration, defaults will be used"))

  // TODO improve configuration style
  val devMode = !args.contains("prod")

  def launch(port: Int):Option[Server] = {
    logger.info(s"Starting web server on port $port")
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
