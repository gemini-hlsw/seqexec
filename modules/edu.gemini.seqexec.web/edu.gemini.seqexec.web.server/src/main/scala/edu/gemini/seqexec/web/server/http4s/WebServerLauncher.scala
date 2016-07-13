package edu.gemini.seqexec.web.server.http4s

import java.io.File
import java.util.logging.Logger

import edu.gemini.seqexec.web.server.common.LogInitialization
import knobs._
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze.BlazeBuilder

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO

object WebServerLauncher extends ServerApp with LogInitialization {
  // Initialize the log and exit if it fails
  configLog.run.onException(IO(sys.exit(1))).unsafePerformIO()

  // Initialize logger after the configuration
  val logger = Logger.getLogger(getClass.getName)

  case class ServerConfiguration(host: String, port: Int, devMode: Boolean)

  // Attempt to get the file or throw an exception if not possible
  val configurationFile: File = new File(new File(baseDir, "conf"), "app.conf")

  // Read the config, first attempt the file or default to the classpath file
  val config: Task[Config] = knobs.loadImmutable(
    knobs.Optional(FileResource(configurationFile)) :: Required(ClassPathResource("app.conf")) :: Nil)

  // configuration specific to the web server
  val serverConf: Task[ServerConfiguration] =
    config.map { cfg =>
      val host = cfg.require[String]("web-server.host")
      val port = cfg.require[Int]("web-server.port")
      val devMode = cfg.require[String]("mode")
      ServerConfiguration(host, port, devMode.equalsIgnoreCase("dev"))
    }

  override def server(args: List[String]): Task[Server] = {
    serverConf >>= { conf =>
      logger.info(s"Start server on ${conf.devMode ? "dev" | "production"} mode")
      BlazeBuilder.bindHttp(conf.port, conf.host)
        .withWebSockets(true)
        .mountService(StaticRoutes.service(conf.devMode), "/")
        .mountService(SeqexecCommandRoutes.service, "/api/seqexec/commands")
        .mountService(SeqexecUIApiRoutes.service, "/api")
        .start
    }
  }

}
