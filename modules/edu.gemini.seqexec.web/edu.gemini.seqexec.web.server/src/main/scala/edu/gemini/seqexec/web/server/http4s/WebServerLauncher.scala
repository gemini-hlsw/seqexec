// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server.http4s

import org.log4s._

import edu.gemini.seqexec.engine
import edu.gemini.seqexec.server
import edu.gemini.seqexec.model.Model.SeqexecEvent
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.web.server.OcsBuildInfo
import edu.gemini.seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
import edu.gemini.web.server.common.{LogInitialization, StaticRoutes, RedirectToHttpsRoutes}
import knobs._
import org.http4s.server.SSLKeyStoreSupport.StoreInfo
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.Server
import org.http4s.util.ProcessApp
import squants.time.Hours

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.concurrent.Task._
import scalaz.stream.Process
import scalaz.stream.async
import scalaz.stream.async.mutable.Topic

object WebServerLauncher extends ProcessApp with LogInitialization {
  case class SSLConfig(keyStore: String, keyStorePwd: String, certPwd: String)

  /**
    * Configuration for the web server
    */
  case class WebServerConfiguration(site: String, host: String, port: Int, insecurePort: Int, externalBaseUrl: String, devMode: Boolean, sslConfig: Option[SSLConfig])

  // Attempt to get the configuration file relative to the base dir
  val configurationFile: Task[java.nio.file.Path] = baseDir.map(_.resolve("conf").resolve("app.conf"))

  // Read the config, first attempt the file or default to the classpath file
  val defaultConfig: Task[Config] =
    knobs.loadImmutable(ClassPathResource("app.conf").required :: Nil)

  val fileConfig: Task[Config] = configurationFile >>= { f =>
    knobs.loadImmutable(FileResource(f.toFile).optional :: Nil)
  }

  val config: Task[Config] =
    for {
      dc <- defaultConfig
      fc <- fileConfig
    } yield dc ++ fc

  // configuration specific to the web server
  val serverConf: Task[WebServerConfiguration] =
    config.map { cfg =>
      val site            = cfg.require[String]("seqexec-engine.site")
      val host            = cfg.require[String]("web-server.host")
      val port            = cfg.require[Int]("web-server.port")
      val insecurePort    = cfg.require[Int]("web-server.insecurePort")
      val externalBaseUrl = cfg.require[String]("web-server.externalBaseUrl")
      val devMode         = cfg.require[String]("mode")
      val keystore        = cfg.lookup[String]("web-server.tls.keyStore")
      val keystorePwd     = cfg.lookup[String]("web-server.tls.keyStorePwd")
      val certPwd         = cfg.lookup[String]("web-server.tls.certPwd")
      val sslConfig       = (keystore |@| keystorePwd |@| certPwd)(SSLConfig.apply)
      WebServerConfiguration(site, host, port, insecurePort, externalBaseUrl, devMode.equalsIgnoreCase("dev"), sslConfig)
    }

  // Configuration of the ldap clients
  val ldapConf: Task[LDAPConfig] =
    config.map { cfg =>
      val urls = cfg.require[List[String]]("authentication.ldapURLs")
      LDAPConfig(urls)
    }

  // Configuration of the authentication service
  val authConf: Kleisli[Task, WebServerConfiguration, AuthenticationConfig] = Kleisli { conf =>
    for {
      ld <- ldapConf
      cfg <- config
    } yield {
      val devMode = cfg.require[String]("mode")
      val sessionTimeout = cfg.require[Int]("authentication.sessionLifeHrs")
      val cookieName = cfg.require[String]("authentication.cookieName")
      val secretKey = cfg.require[String]("authentication.secretKey")
      val sslSettings = cfg.lookup[String]("web-server.tls.keyStore")
      AuthenticationConfig(devMode.equalsIgnoreCase("dev"), Hours(sessionTimeout), cookieName, secretKey, sslSettings.isDefined, ld)
    }
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
  def webServer(as: AuthenticationService, events: (server.EventQueue, Topic[SeqexecEvent]), se: SeqexecEngine): Kleisli[Task, WebServerConfiguration, Server] = Kleisli { conf =>
    val logger = getLogger
    logger.info(s"Start web server for site ${conf.site} on ${conf.devMode ? "dev" | "production"} mode")

    val builder = BlazeBuilder.bindHttp(conf.port, conf.host)
      .withWebSockets(true)
      .mountService(new StaticRoutes(index(conf.site, conf.devMode, OcsBuildInfo.builtAtMillis), conf.devMode, OcsBuildInfo.builtAtMillis).service, "/")
      .mountService(new SeqexecCommandRoutes(as, events._1, se).service, "/api/seqexec/commands")
      .mountService(new SeqexecUIApiRoutes(as, events, se).service, "/api")
    conf.sslConfig.fold(builder) { ssl =>
      val storeInfo = StoreInfo(ssl.keyStore, ssl.keyStorePwd)
      builder.withSSL(storeInfo, ssl.certPwd, "TLS")
    }.start
  }

  def redirectWebServer: Kleisli[Task, WebServerConfiguration, Server] = Kleisli { conf =>
    val builder = BlazeBuilder.bindHttp(conf.insecurePort, conf.host)
      .mountService(RedirectToHttpsRoutes(443, conf.externalBaseUrl).service, "/")
    builder.start
  }

  /**
    * Reads the configuration and launches the web server
    */
  override def process(args: List[String]): Process[Task, Nothing] = {
    val engineTask = for {
      _    <- configLog // Initialize log before the engine is setup
      c    <- config
      seqc <- SeqexecEngine.seqexecConfiguration.run(c)
    } yield SeqexecEngine(seqc)

    val inq  = async.boundedQueue[engine.Event](10)
    val out  = async.topic[SeqexecEvent]()

    // It should be possible to cleanup the engine at shutdown in this function
    def cleanup = (s: SeqexecEngine) => Process.eval_(Task.now(()))
    Process.bracket(engineTask)(cleanup) { case et =>
        val pt = Nondeterminism[Task].both(
          // Launch engine and broadcast channel
          et.eventProcess(inq).to(out.publish).run,
          // Launch web server
          for {
            wc <- serverConf
            ac <- authConf.run(wc)
            as <- authService.run(ac)
            rd <- redirectWebServer.run(wc)
            ws <- webServer(as, (inq, out), et).run(wc)
          } yield (ws, rd)
        )
      Process.eval_(pt.map(_._2))
    }
  }

}
