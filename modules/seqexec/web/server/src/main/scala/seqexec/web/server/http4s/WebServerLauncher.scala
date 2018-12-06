// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.data.Kleisli
import cats.effect.IO
import cats.implicits._
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.Appender
import fs2.async.mutable.Queue
import fs2.StreamApp.ExitCode
import fs2.async.mutable.Topic
import fs2.{Scheduler, Stream, StreamApp, async}
import gem.enum.Site
import io.prometheus.client.CollectorRegistry
import knobs._
import mouse.all._
import org.http4s.server.SSLKeyStoreSupport.StoreInfo
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.client.blaze._
import org.http4s.client.Client
import org.http4s.HttpService
import org.http4s.server.Router
import org.http4s.server.prometheus.{PrometheusMetrics, PrometheusExportService}
import org.http4s.server.prometheus.{PrometheusExportService}
import org.log4s._
import scala.concurrent.ExecutionContext.Implicits.global
import seqexec.model.events._
import seqexec.server
import seqexec.server.{SeqexecMetrics, SeqexecConfiguration, SeqexecEngine, executeEngine}
import seqexec.server.GpiSettings
import seqexec.server.GhostSettings
import seqexec.web.server.OcsBuildInfo
import seqexec.web.server.logging.AppenderForClients
import seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
import squants.time.Hours
import web.server.common.{LogInitialization, RedirectToHttpsRoutes, StaticRoutes}

object WebServerLauncher extends StreamApp[IO] with LogInitialization with SeqexecConfiguration {
  private val logger = getLogger

  final case class SSLConfig(keyStore: String, keyStorePwd: String, certPwd: String)

  /**
    * Configuration for the web server
    */
  final case class WebServerConfiguration(site: String, host: String, port: Int, insecurePort: Int, externalBaseUrl: String, devMode: Boolean, sslConfig: Option[SSLConfig])

  // Attempt to get the configuration file relative to the base dir
  val configurationFile: IO[java.nio.file.Path] = baseDir.map(_.resolve("conf").resolve("app.conf"))

  // Read the config, first attempt the file or default to the classpath file
  val defaultConfig: IO[Config] =
    knobs.loadImmutable[IO](ClassPathResource("app.conf").required :: Nil)

  val fileConfig: IO[Config] = configurationFile >>= { f =>
    knobs.loadImmutable[IO](FileResource(f.toFile).optional :: Nil)
  }

  val config: IO[Config] =
    for {
      dc <- defaultConfig
      fc <- fileConfig
    } yield dc ++ fc

  // configuration specific to the web server
  val serverConf: IO[WebServerConfiguration] =
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
      val sslConfig       = (keystore, keystorePwd, certPwd).mapN(SSLConfig.apply)
      WebServerConfiguration(site, host, port, insecurePort, externalBaseUrl, devMode.equalsIgnoreCase("dev"), sslConfig)
    }

  // Configuration of the ldap clients
  val ldapConf: IO[LDAPConfig] =
    config.map { cfg =>
      val urls = cfg.require[List[String]]("authentication.ldapURLs")
      LDAPConfig(urls)
    }

  // Configuration of the authentication service
  val authConf: Kleisli[IO, WebServerConfiguration, AuthenticationConfig] = Kleisli { _ =>
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
  def authService: Kleisli[IO, AuthenticationConfig, AuthenticationService] = Kleisli { conf =>
    IO.apply(AuthenticationService(conf))
  }

  /**
    * Configures and builds the web server
    */
  def webServer(as: AuthenticationService, inputs: server.EventQueue, outputs: Topic[IO, SeqexecEvent], se: SeqexecEngine, pe: PrometheusExportService[IO]): Kleisli[Stream[IO, ?], WebServerConfiguration, StreamApp.ExitCode] = Kleisli { conf =>
    val metricsMiddleware = PrometheusMetrics[IO](pe.collectorRegistry)

    def build(all: HttpService[IO]): Stream[IO, StreamApp.ExitCode] = {
      val builder = BlazeBuilder[IO].bindHttp(conf.port, conf.host)
        .withWebSockets(true)
        .mountService(all)
      conf.sslConfig.fold(builder) { ssl =>
        val storeInfo = StoreInfo(ssl.keyStore, ssl.keyStorePwd)
        builder.withSSL(storeInfo, ssl.certPwd, "TLS")
      }.serve
    }

    val router = Router[IO](
            "/" -> new StaticRoutes(conf.devMode, OcsBuildInfo.builtAtMillis).service,
            "/api/seqexec/commands" -> new SeqexecCommandRoutes(as, inputs, se).service,
            "/api" -> new SeqexecUIApiRoutes(conf.site, conf.devMode, as, outputs).service)
    for {
      static <- Stream.eval(metricsMiddleware(router))
      blaze <- build(pe.service <+> static)
    } yield blaze
  }

  def redirectWebServer: Kleisli[Stream[IO, ?], WebServerConfiguration, StreamApp.ExitCode] = Kleisli { conf =>
    val builder = BlazeBuilder[IO].bindHttp(conf.insecurePort, conf.host)
      .mountService(new RedirectToHttpsRoutes(443, conf.externalBaseUrl).service, "/")
    builder.serve
  }

  def logStart: Kleisli[IO, WebServerConfiguration, Unit] = Kleisli { conf =>
    val msg = s"Start web server for site ${conf.site} on ${conf.devMode.fold("dev", "production")} mode"
    IO.apply { logger.info(msg) }
  }

  // We need to manually update the configuration of the logging subsystem
  // to support capturing log messages and forward them to the clients
  def logToClients(out: Topic[IO, SeqexecEvent]): IO[Appender[ILoggingEvent]] = IO.apply {
    import ch.qos.logback.classic.{AsyncAppender, Logger, LoggerContext}
    import org.slf4j.LoggerFactory

    val asyncAppender = new AsyncAppender
    val appender = new AppenderForClients(out)
    Option(LoggerFactory.getILoggerFactory).collect {
      case lc: LoggerContext => lc
    }.foreach { ctx =>
      asyncAppender.setContext(ctx)
      appender.setContext(ctx)
      asyncAppender.addAppender(appender)
    }

    Option(LoggerFactory.getLogger("seqexec")).collect {
      case l: Logger => l
    }.foreach { l =>
      l.addAppender(asyncAppender)
      asyncAppender.start()
      appender.start()
    }
    asyncAppender
  }

  /**
    * Reads the configuration and launches the web server
    */
  def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    def engineIO(httpClient: Client[IO], collector: CollectorRegistry): IO[SeqexecEngine] =
      for {
        _          <- configLog // Initialize log before the engine is setup
        c          <- config
        site       <- IO.pure(c.require[Site]("seqexec-engine.site"))
        giapiGPI   <- SeqexecEngine.giapiConnection[GpiSettings]("seqexec-engine.systemControl.gpi",
                                                    "seqexec-engine.gpiUrl").run(c)
        giapiGHOST <- SeqexecEngine.giapiConnection[GhostSettings]("seqexec-engine.systemControl.ghost",
                                                    "seqexec-engine.ghostUrl").run(c)
        seqc       <- SeqexecEngine.seqexecConfiguration(giapiGPI, giapiGHOST).run(c)
        met        <- SeqexecMetrics.build[IO](site, collector)
      } yield SeqexecEngine(httpClient, seqc, met)

    def webServerIO(in: Queue[IO, executeEngine.EventType], out: Topic[IO, SeqexecEvent], et: SeqexecEngine, pe: PrometheusExportService[IO]): IO[Stream[IO, ExitCode]] =
      // Launch web server
      for {
        wc <- serverConf
        ac <- authConf.run(wc)
        as <- authService.run(ac)
        _  <- logStart.run(wc)
        _  <- logToClients(out)
      } yield Stream(redirectWebServer.run(wc), webServer(as, in, out, et, pe).run(wc)).join(2)

    // I have taken this from the examples at:
    // https://github.com/gvolpe/advanced-http4s/blob/master/src/main/scala/com/github/gvolpe/fs2/PubSub.scala
    // It's not very clear why we need to run this inside a Scheduler
    Scheduler[IO](corePoolSize = 4).flatMap { _ =>
      for {
        cli    <- Http1Client.stream[IO]()
        inq    <- Stream.eval(async.boundedQueue[IO, executeEngine.EventType](10))
        out    <- Stream.eval(async.topic[IO, SeqexecEvent](NullEvent))
        pe     <- Stream.eval(PrometheusExportService.build[IO])
        engine <- Stream.eval(engineIO(cli, pe.collectorRegistry))
        web    <- Stream.eval(webServerIO(inq, out, engine, pe))
        exit   <- Stream(
                    engine.eventStream(inq).to(out.publish),
                    web
                  ).join(2).drain ++ Stream.emit(ExitCode.Success)
      } yield exit
    }
  }

}
