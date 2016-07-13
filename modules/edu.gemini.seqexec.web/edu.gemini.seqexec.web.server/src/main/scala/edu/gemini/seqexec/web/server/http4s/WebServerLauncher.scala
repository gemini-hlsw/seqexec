package edu.gemini.seqexec.web.server.http4s

import java.io.File
import java.util.logging.Logger

import edu.gemini.seqexec.web.server.common.LogInitialization
import knobs.{ClassPathResource, Config, FileResource, Required}
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeBuilder

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO

object WebServerLauncher extends App with LogInitialization {
  // Initialize the log and exit if it fails
  configLog.run.onException(IO(sys.exit(1))).unsafePerformIO()

  // Initialize logger after the configuration
  val Log = Logger.getLogger(WebServerLauncher.getClass.getSimpleName)

  // Attempt to get the file or throw an exception if not possible
  def configurationFile: File = new File(new File(baseDir, "conf"), "app.cfg")

  // Read the config, first attempt the file or default to the classpath file
  val config: Task[Config] = knobs.loadImmutable(
    knobs.Optional(FileResource(configurationFile)) :: Required(ClassPathResource("app.cfg")) :: Nil)

  val logger = Logger.getLogger(getClass.getName)

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
