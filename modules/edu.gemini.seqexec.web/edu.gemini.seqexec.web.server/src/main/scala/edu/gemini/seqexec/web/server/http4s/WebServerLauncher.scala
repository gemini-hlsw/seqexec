package edu.gemini.seqexec.web.server.http4s

import java.io.File
import java.util.logging.Logger

import edu.gemini.seqexec.server.ExecutorImpl
import edu.gemini.seqexec.web.server.common.LogInitialization
import edu.gemini.seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
import edu.gemini.spModel.core.Peer
import knobs._
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze.BlazeBuilder

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object WebServerLauncher extends ServerApp with LogInitialization {

  /**
    * Configuration for the web server
    */
  case class WebServerConfiguration(host: String, port: Int, devMode: Boolean)

  /**
    * Configuration for the seqexec engine
    */
  case class SeqexecConfiguration(odbHost: String)

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

  val executorConf: Task[SeqexecConfiguration] =
    config.map { cfg =>
      val host = cfg.require[String]("seqexec-engine.odb")
      SeqexecConfiguration(host)
    }

  /**
    * Configures the Seqexec executor
    */
  def seqexecExecutor: Kleisli[Task, SeqexecConfiguration, Unit] = Kleisli { conf =>
    Task.delay(ExecutorImpl.host(new Peer(conf.odbHost, 8443, null)))
  }

  /**
    * Configures the Authentication service
    */
  def authService: Kleisli[Task, AuthenticationConfig, AuthenticationService] = Kleisli { conf =>
    Task.delay(AuthenticationService(conf))
  }

  /**
    * Configures and builds the web server
    */
  def webServer(as: AuthenticationService): Kleisli[Task, WebServerConfiguration, Server] = Kleisli { conf =>
    val logger = Logger.getLogger(getClass.getName)
    logger.info(s"Start server on ${conf.devMode ? "dev" | "production"} mode")

    BlazeBuilder.bindHttp(conf.port, conf.host)
      .withWebSockets(true)
      .mountService(new StaticRoutes(conf.devMode).service, "/")
      .mountService(new SeqexecCommandRoutes(as).service, "/api/seqexec/commands")
      .mountService(new SeqexecUIApiRoutes(as).service, "/api")
      .start
  }

  /**
    * Reads the configuration and launches the web server
    */
  override def server(args: List[String]): Task[Server] =
    for {
      _  <- configLog
      ac <- authConf
      wc <- serverConf
      sc <- executorConf
      _  <- seqexecExecutor.run(sc)
      as <- authService.run(ac)
      ws <- webServer(as).run(wc)
    } yield ws
}
