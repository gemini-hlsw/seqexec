// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.data.Kleisli
import cats.effect._
import cats.implicits._
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.Appender
import fs2.concurrent.Queue
import fs2.concurrent.Topic
import gem.enum.Site
import io.prometheus.client.CollectorRegistry
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import java.nio.file.{Path => FilePath}
import java.util.concurrent.{ExecutorService, Executors}
import knobs.{Resource => _, _}
import mouse.all._
import org.asynchttpclient.DefaultAsyncHttpClientConfig
import org.http4s.client.asynchttpclient.AsyncHttpClient
import org.http4s.client.Client
import org.http4s.HttpRoutes
import org.http4s.metrics.prometheus.Prometheus
import org.http4s.metrics.prometheus.PrometheusExportService
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Metrics
import org.http4s.server.middleware.{Logger => Http4sLogger}
import org.http4s.server.Router
import org.http4s.server.Server
import org.http4s.server.SSLKeyStoreSupport.StoreInfo
import org.http4s.syntax.kleisli._
import scala.concurrent.ExecutionContext
import seqexec.model.events._
import seqexec.server
import seqexec.server.tcs.GuideConfigDb
import seqexec.server.{SeqexecConfiguration, SeqexecEngine, SeqexecMetrics, executeEngine}
import seqexec.server.SeqexecFailure
import seqexec.server.Systems
import seqexec.web.server.OcsBuildInfo
import seqexec.web.server.logging.AppenderForClients
import seqexec.web.server.security.{AuthenticationConfig, AuthenticationService, LDAPConfig}
import squants.time.Hours
import web.server.common.{LogInitialization, RedirectToHttpsRoutes, StaticRoutes}

object WebServerLauncher extends IOApp with LogInitialization with SeqexecConfiguration {
  private implicit def L: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("seqexec")

  final case class SSLConfig(keyStore: String, keyStorePwd: String, certPwd: String)

  /** Configuration for the web server */
  final case class WebServerConfiguration(
    site:              String,
    host:              String,
    port:              Int,
    insecurePort:      Int,
    externalBaseUrl:   String,
    devMode:           Boolean,
    sslConfig:         Option[SSLConfig],
    smartGCalHost:     String,
    smartGCalLocation: String
  )

  // Attempt to get the configuration file relative to the base dir
  val configurationFile: IO[FilePath] =
    baseDir.map(_.resolve("conf").resolve("app.conf"))

  // Read the config, first attempt the file or default to the classpath file
  val defaultConfig: IO[Config] =
    knobs.loadImmutable[IO](ClassPathResource("app.conf").required :: Nil)

  val fileConfig: IO[Config] =
    configurationFile.flatMap { f =>
      knobs.loadImmutable[IO](FileResource(f.toFile).optional :: Nil)
    }

  val config: IO[Config] =
    (defaultConfig, fileConfig).mapN(_ ++ _)

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
      val smartGCalHost   = cfg.require[String]("seqexec-engine.smartGCalHost")
      val smartGCalDir    = cfg.require[String]("seqexec-engine.smartGCalDir")

      WebServerConfiguration(
        site,
        host,
        port,
        insecurePort,
        externalBaseUrl,
        devMode.equalsIgnoreCase("dev"),
        sslConfig,
        smartGCalHost,
        smartGCalDir
      )

    }

  // Configuration of the ldap clients
  val ldapConf: IO[LDAPConfig] =
    config.map { cfg =>
      val urls = cfg.require[List[String]]("authentication.ldapURLs")
      LDAPConfig(urls)
    }

  // Configuration of the authentication service
  val authConf: IO[AuthenticationConfig] =
    (ldapConf, config).mapN { (ld, cfg) =>

      val devMode        = cfg.require[String]("mode")
      val sessionTimeout = cfg.require[Int]("authentication.sessionLifeHrs")
      val cookieName     = cfg.require[String]("authentication.cookieName")
      val secretKey      = cfg.require[String]("authentication.secretKey")
      val sslSettings    = cfg.lookup[String]("web-server.tls.keyStore")

      AuthenticationConfig(
        devMode.equalsIgnoreCase("dev"),
        Hours(sessionTimeout),
        cookieName,
        secretKey,
        sslSettings.isDefined,
        ld
      )

    }

  /** Configures the Authentication service */
  def authService(conf: AuthenticationConfig): IO[AuthenticationService] =
    IO.apply(AuthenticationService(conf))

  /** Resource that yields the running web server */
  def webServer(
    as: AuthenticationService,
    cal: SmartGcal,
    inputs: server.EventQueue[IO],
    outputs: Topic[IO, SeqexecEvent],
    se: SeqexecEngine,
    cr: CollectorRegistry,
    bec: ExecutionContext
  )(conf: WebServerConfiguration): Resource[IO, Server[IO]] = {

    // The prometheus route does not get logged
    val prRouter = Router[IO](
      "/"                     -> PrometheusExportService[IO](cr).routes
    )

    def build(all: IO[HttpRoutes[IO]]): Resource[IO, Server[IO]] = Resource.liftF(all).flatMap { all =>

      val builder =
        BlazeServerBuilder[IO]
          .bindHttp(conf.port, conf.host)
          .withWebSockets(true)
          .withHttpApp((prRouter <+> all).orNotFound)

      conf.sslConfig.fold(builder) { ssl =>
        val storeInfo = StoreInfo(ssl.keyStore, ssl.keyStorePwd)
        builder.withSSL(storeInfo, ssl.certPwd, "TLS")
      }.resource

    }

    val router = Router[IO](
      "/"                     -> new StaticRoutes(conf.devMode, OcsBuildInfo.builtAtMillis, bec).service,
      "/api/seqexec/commands" -> new SeqexecCommandRoutes(as, inputs, se).service,
      "/api"                  -> new SeqexecUIApiRoutes(conf.site, conf.devMode, as, se.systems.guideDb, se.systems.gpi.statusDb, outputs).service,
      "/api/seqexec/guide"    -> new GuideConfigDbRoutes(se.systems.guideDb).service,
      "/smartgcal"            -> new SmartGcalRoutes[IO](cal).service
    )

    val loggedRoutes = Http4sLogger.httpRoutes(logHeaders = false, logBody = false)(router)
    val metricsMiddleware = Prometheus[IO](cr, "seqexec").map(
      Metrics[IO](_)(loggedRoutes))

    build(metricsMiddleware)

  }

  def redirectWebServer(
    gcdb: GuideConfigDb[IO],
    cal: SmartGcal
  )(conf: WebServerConfiguration): Resource[IO, Server[IO]] = {
    val router = Router[IO](
      "/api/seqexec/guide" -> new GuideConfigDbRoutes(gcdb).service,
      "/smartgcal"         -> new SmartGcalRoutes[IO](cal).service,
      "/"                  -> new RedirectToHttpsRoutes(443, conf.externalBaseUrl).service
    )

    BlazeServerBuilder[IO]
      .bindHttp(conf.insecurePort, conf.host)
      .withHttpApp(router.orNotFound)
      .resource
  }

  def logStart: Kleisli[IO, WebServerConfiguration, Unit] = Kleisli { conf =>
    val banner = """
   _____
  / ___/___  ____ ____  _  _____  _____
  \__ \/ _ \/ __ `/ _ \| |/_/ _ \/ ___/
 ___/ /  __/ /_/ /  __/>  </  __/ /__
/____/\___/\__, /\___/_/|_|\___/\___/
             /_/
"""
    val msg = s"""Start web server for site ${conf.site} on ${conf.devMode.fold("dev", "production")} mode, version ${OcsBuildInfo.version}"""
    L.info(banner + msg)
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

  // Logger of error of last resort.
  def logError: PartialFunction[Throwable, IO[Unit]] = {
    case e: SeqexecFailure => L.error(e)(s"Seqexec global error handler ${SeqexecFailure.explain(e)}")
    case e: Exception      => L.error(e)("Seqexec global error handler")
  }

  /** Reads the configuration and launches the web server */
  def run(args: List[String]): IO[ExitCode] = {

    def blockingExecutionContext: Resource[IO, ExecutionContext] = {
      val alloc = IO(Executors.newCachedThreadPool)
      val free  = (es: ExecutorService) => IO(es.shutdown())
      Resource.make(alloc)(free).map(ExecutionContext.fromExecutor)
    }

    // Override the default client config
    val clientConfig = new DefaultAsyncHttpClientConfig.Builder(AsyncHttpClient.defaultConfig)
      .setRequestTimeout(5000) // Change the timeout to 5 seconds
      .build()

    def engineIO(
      httpClient: Client[IO],
      collector: CollectorRegistry
    ): Resource[IO, SeqexecEngine] =
      for {
        cfg  <- Resource.liftF(config)
        site <- Resource.liftF(IO(cfg.require[Site]("seqexec-engine.site")))
        seqc <- Resource.liftF(SeqexecEngine.seqexecConfiguration.run(cfg))
        met  <- Resource.liftF(SeqexecMetrics.build[IO](site, collector))
        sys  <- Systems.build(httpClient, seqc)
      } yield SeqexecEngine(sys, seqc, met)

    def webServerIO(
      in:  Queue[IO, executeEngine.EventType],
      out: Topic[IO, SeqexecEvent],
      en:  SeqexecEngine,
      cr:  CollectorRegistry,
      bec: ExecutionContext
    ): Resource[IO, Unit] =
      for {
        wc <- Resource.liftF(serverConf)
        ac <- Resource.liftF(authConf)
        as <- Resource.liftF(authService(ac))
        _  <- Resource.liftF(logStart.run(wc))
        _  <- Resource.liftF(logToClients(out))
        ca <- Resource.liftF(SmartGcalInitializer.init[IO](wc.smartGCalHost, wc.smartGCalLocation))
        _  <- redirectWebServer(en.systems.guideDb, ca)(wc)
        _  <- webServer(as, ca, in, out, en, cr, bec)(wc)
      } yield ()

    val r: Resource[IO, ExitCode] =
      for {
        _      <- Resource.liftF(configLog) // Initialize log before the engine is setup
        cli    <- AsyncHttpClient.resource[IO](clientConfig)
        inq    <- Resource.liftF(Queue.bounded[IO, executeEngine.EventType](10))
        out    <- Resource.liftF(Topic[IO, SeqexecEvent](NullEvent))
        cr     <- Resource.liftF(IO(new CollectorRegistry))
        bec    <- blockingExecutionContext
        engine <- engineIO(cli, cr)
        _      <- webServerIO(inq, out, engine, cr, bec)
        f      <- Resource.liftF(engine.eventStream(inq).through(out.publish).compile.drain.onError(logError).start)
        _      <- Resource.liftF(f.join) // We need to join to catch uncaught errors
      } yield ExitCode.Success

    r.use(_ => IO.never)

  }

}
