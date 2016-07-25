package edu.gemini.seqexec.web.server.http4s

import java.io.File
import java.util.logging.Logger

import edu.gemini.seqexec.web.server.common.LogInitialization
import edu.gemini.seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
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

  case class WebServerConfiguration(host: String, port: Int, devMode: Boolean)

  // Attempt to get the file or throw an exception if not possible
  val configurationFile: File = new File(new File(baseDir, "conf"), "app.conf")

  // Read the config, first attempt the file or default to the classpath file
  val config: Task[Config] = knobs.loadImmutable(
    Required(FileResource(configurationFile) or ClassPathResource("app.conf")) :: Nil)

  // configuration specific to the web server
  val serverConf: Task[WebServerConfiguration] =
    config.map { cfg =>
      val host = cfg.require[String]("web-server.host")
      val port = cfg.require[Int]("web-server.port")
      val devMode = cfg.require[String]("mode")
      WebServerConfiguration(host, port, devMode.equalsIgnoreCase("dev"))
    }

  // Configuration of the ldap clients
  val ldapConf: Task[LDAPConfig] =
    config.map { cfg =>
      val urls = cfg.require[List[String]]("authentication.ldapURLs")
      LDAPConfig(urls)
    }

  // Configuration of the authentication service
  val authConf: Task[AuthenticationConfig] =
    for {
      ld <- ldapConf
      cfg <- config
    } yield {
      val devMode = cfg.require[String]("mode")
      val sessionTimeout = cfg.require[Int]("authentication.sessionLifeHrs")
      val cookieName = cfg.require[String]("authentication.cookieName")
      val secretKey = cfg.require[String]("authentication.secretKey")
      val useSSL = cfg.require[Boolean]("authentication.useSSL")
      AuthenticationConfig(devMode.equalsIgnoreCase("dev"), sessionTimeout, cookieName, secretKey, useSSL, ld)
    }

  /**
    * Configures and builds the web server
    */
  def webServer(as: AuthenticationService): Kleisli[Task, WebServerConfiguration, Server] = Kleisli { conf =>
    logger.info(s"Start server on ${conf.devMode ? "dev" | "production"} mode")
    BlazeBuilder.bindHttp(conf.port, conf.host)
      .withWebSockets(true)
      .mountService(StaticRoutes.service(conf.devMode), "/")
      .mountService(SeqexecCommandRoutes.service, "/api/seqexec/commands")
      .mountService(new SeqexecUIApiRoutes(as).service, "/api")
      .start
  }

  /**
    * Reads the configuration and launches the web server
    */
  override def server(args: List[String]): Task[Server] =
    for {
      ac <- authConf
      wc <- serverConf
      as <- AuthenticationService.authServices.run(ac)
      ws <- webServer(as).run(wc)
    } yield ws
}
