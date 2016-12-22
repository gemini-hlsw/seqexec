package edu.gemini.p1backend.server.http4s

import java.io.File
import java.util.logging.Logger

import edu.gemini.p1backend.server.OcsBuildInfo
import edu.gemini.web.server.common.{LogInitialization, StaticRoutes}
import knobs._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.{Server, ServerApp}

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

object WebServerLauncher extends ServerApp with LogInitialization {

  /**
    * Configuration for the web server
    */
  case class WebServerConfiguration(host: String, port: Int, devMode: Boolean)

  // Attempt to get the configuration file relative to the base dir
  val configurationFile: Task[File] = baseDir.map(f => new File(new File(f, "conf"), "app.conf"))

  // Read the config, first attempt the file or default to the classpath file
  val defaultConfig: Task[Config] =
    knobs.loadImmutable(ClassPathResource("app.conf").required :: Nil)

  val fileConfig: Task[Config] = configurationFile >>= { f =>
    knobs.loadImmutable(FileResource(f).optional :: Nil)
  }

  val config: Task[Config] =
    for {
      dc <- defaultConfig
      fc <- fileConfig
    } yield dc ++ fc

  // configuration specific to the web server
  val serverConf: Task[WebServerConfiguration] =
    config.map { cfg =>
      val host = cfg.require[String]("web-server.host")
      val port = cfg.require[Int]("web-server.port")
      val devMode = cfg.require[String]("mode")
      WebServerConfiguration(host, port, devMode.equalsIgnoreCase("dev"))
    }

  /**
    * Configures and builds the web server
    */
  def webServer: Kleisli[Task, WebServerConfiguration, Server] = Kleisli { conf =>
    val logger = Logger.getLogger(getClass.getName)
    logger.info(s"Start server on ${conf.devMode ? "dev" | "production"} mode")

    BlazeBuilder.bindHttp(conf.port, conf.host)
      .withWebSockets(true)
      .mountService(new StaticRoutes(index(conf.devMode, OcsBuildInfo.builtAtMillis), conf.devMode, OcsBuildInfo.builtAtMillis).service, "/")
      .start
  }

  /**
    * Reads the configuration and launches the web server
    */
  override def server(args: List[String]): Task[Server] = {
    // Launch web server
    for {
      _  <- configLog
      sc <- serverConf
      ws <- webServer.run(sc)
    } yield ws
  }

}
