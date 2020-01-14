// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect._
import cats.implicits._
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.Appender
import fs2.concurrent.InspectableQueue
import fs2.concurrent.Queue
import fs2.concurrent.Topic
import io.prometheus.client.CollectorRegistry
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import java.nio.file.{Path => FilePath}
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
import pureconfig._
import seqexec.model.events._
import seqexec.server
import seqexec.server.tcs.GuideConfigDb
import seqexec.model.config._
import seqexec.web.server.config._
import seqexec.server.{SeqexecEngine, SeqexecMetrics, executeEngine}
import seqexec.server.SeqexecFailure
import seqexec.server.Systems
import seqexec.server.CaServiceInit
import seqexec.web.server.OcsBuildInfo
import seqexec.web.server.config._
import seqexec.web.server.logging.AppenderForClients
import seqexec.web.server.security.AuthenticationService
import web.server.common.{LogInitialization, RedirectToHttpsRoutes, StaticRoutes}

object WebServerLauncher extends IOApp with LogInitialization {
  private implicit def L: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("seqexec")

  // Attempt to get the configuration file relative to the base dir
  def configurationFile[F[_]: Sync]: F[FilePath] =
    baseDir[F].map(_.resolve("conf").resolve("app.conf"))

  // Try to load config from the file and fall back to the common one in the class path
  def config[F[_]: Sync]: F[ConfigObjectSource] = {
    val defaultConfig = ConfigSource.resources("app.conf").pure[F]
    val fileConfig = configurationFile.map(ConfigSource.file)

    // ConfigSource, first attempt the file or default to the classpath file
    (fileConfig, defaultConfig).mapN(_.optional.withFallback(_))
  }

  /** Configures the Authentication service */
  def authService[F[_]: Sync: Timer: Logger](mode: Mode, conf: AuthenticationConfig): F[AuthenticationService[F]] =
    Sync[F].delay(AuthenticationService[F](mode, conf))

  /** Resource that yields the running web server */
  def webServer[F[_]: ContextShift: Logger: ConcurrentEffect: Timer](
    conf: SeqexecConfiguration,
    cal: SmartGcal,
    as: AuthenticationService[F],
    inputs: server.EventQueue[F],
    outputs: Topic[F, SeqexecEvent],
    se: SeqexecEngine[F],
    cr: CollectorRegistry,
    bec: Blocker
  ): Resource[F, Server[F]] = {

    // The prometheus route does not get logged
    val prRouter = Router[F](
      "/" -> PrometheusExportService[F](cr).routes
    )

    def build(all: F[HttpRoutes[F]]): Resource[F, Server[F]] = Resource.liftF(all).flatMap { all =>

      val builder =
        BlazeServerBuilder[F]
          .bindHttp(conf.webServer.port, conf.webServer.host)
          .withWebSockets(true)
          .withNio2(true)
          .withHttpApp((prRouter <+> all).orNotFound)

      conf.webServer.tls.fold(builder) { tls =>
        val storeInfo = StoreInfo(tls.keyStore.toFile.getAbsolutePath, tls.keyStorePwd)
        builder.withSSL(storeInfo, tls.certPwd, "TLS")
      }.resource

    }

    val router = Router[F](
      "/"                     -> new StaticRoutes(conf.mode === Mode.Development, OcsBuildInfo.builtAtMillis, bec).service,
      "/api/seqexec/commands" -> new SeqexecCommandRoutes(as, inputs, se).service,
      "/api"                  -> new SeqexecUIApiRoutes(conf.site, conf.mode, as, se.systems.guideDb, se.systems.gpi.statusDb, outputs).service,
      "/api/seqexec/guide"    -> new GuideConfigDbRoutes(se.systems.guideDb).service,
      "/smartgcal"            -> new SmartGcalRoutes[F](cal).service
    )

    val pingRouter = Router[F](
      "/ping" -> new PingRoutes(as).service
    )

    val loggedRoutes = pingRouter <+> Http4sLogger.httpRoutes(logHeaders = false, logBody = false)(router)
    val metricsMiddleware: Resource[F, HttpRoutes[F]] = Prometheus.metricsOps[F](cr, "seqexec").map(Metrics[F](_)(loggedRoutes))

    metricsMiddleware.flatMap(x => build(x.pure[F]))

  }

  def redirectWebServer[F[_]: ConcurrentEffect: Logger: Timer](
    gcdb: GuideConfigDb[F],
    cal: SmartGcal
  )(conf: WebServerConfiguration): Resource[F, Server[F]] = {
    val router = Router[F](
      "/api/seqexec/guide" -> new GuideConfigDbRoutes(gcdb).service,
      "/smartgcal"         -> new SmartGcalRoutes[F](cal).service,
      "/"                  -> new RedirectToHttpsRoutes[F](443, conf.externalBaseUrl).service
    )

    BlazeServerBuilder[F]
      .bindHttp(conf.insecurePort, conf.host)
      .withHttpApp(router.orNotFound)
      .withNio2(true)
      .resource
  }

  def printBanner[F[_]: Logger](conf: SeqexecConfiguration): F[Unit] = {
    val banner = """
   _____
  / ___/___  ____ ____  _  _____  _____
  \__ \/ _ \/ __ `/ _ \| |/_/ _ \/ ___/
 ___/ /  __/ /_/ /  __/>  </  __/ /__
/____/\___/\__, /\___/_/|_|\___/\___/
             /_/
"""
    val msg = s"""Start web server for site ${conf.site} on ${conf.mode} mode, version ${OcsBuildInfo.version}"""
    Logger[F].info(banner + msg)
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
  def logError[F[_]: Logger]: PartialFunction[Throwable, F[Unit]] = {
    case e: SeqexecFailure => Logger[F].error(e)(s"Seqexec global error handler ${SeqexecFailure.explain(e)}")
    case e: Exception      => Logger[F].error(e)("Seqexec global error handler")
  }

  /** Reads the configuration and launches the seqexec engine and web server */
  def seqexec: IO[ExitCode] = {

    // Override the default client config
    val clientConfig = new DefaultAsyncHttpClientConfig.Builder(AsyncHttpClient.defaultConfig)
      .setRequestTimeout(5000) // Change the timeout to 5 seconds
      .build()

    def engineIO(
      conf: SeqexecConfiguration,
      httpClient: Client[IO],
      collector: CollectorRegistry
    ): Resource[IO, SeqexecEngine[IO]] =
      for {
        met  <- Resource.liftF(SeqexecMetrics.build[IO](conf.site, collector))
        caS  <- Resource.liftF(CaServiceInit.caInit[IO](conf.seqexecEngine))
        sys  <- Systems.build(conf.site, httpClient, conf.seqexecEngine, caS)
        seqE <- Resource.liftF(SeqexecEngine.build(conf.site, sys, conf.seqexecEngine, met))
      } yield seqE

    def webServerIO(
      conf: SeqexecConfiguration,
      in:  Queue[IO, executeEngine.EventType],
      out: Topic[IO, SeqexecEvent],
      en:  SeqexecEngine[IO],
      cr:  CollectorRegistry
    ): Resource[IO, Unit] =
      for {
        b  <- Blocker[IO]
        as <- Resource.liftF(authService[IO](conf.mode, conf.authentication))
        ca <- Resource.liftF(SmartGcalInitializer.init[IO](conf.smartGcal))
        _  <- redirectWebServer(en.systems.guideDb, ca)(conf.webServer)
        _  <- webServer[IO](conf, ca, as, in, out, en, cr, b)
      } yield ()

    val seqexec: Resource[IO, ExitCode] =
      for {
        _      <- Resource.liftF(configLog[IO]) // Initialize log before the engine is setup
        conf   <- Resource.liftF(config[IO].flatMap(loadConfiguration[IO]))
        _      <- Resource.liftF(printBanner(conf))
        cli    <- AsyncHttpClient.resource[IO](clientConfig)
        inq    <- Resource.liftF(InspectableQueue.bounded[IO, executeEngine.EventType](10))
        out    <- Resource.liftF(Topic[IO, SeqexecEvent](NullEvent))
        _      <- Resource.liftF(logToClients(out))
        cr     <- Resource.liftF(IO(new CollectorRegistry))
        engine <- engineIO(conf, cli, cr)
        _      <- webServerIO(conf, inq, out, engine, cr)
        _      <- Resource.liftF(inq.size.evalMap(l => Logger[IO].debug(s"Queue length: $l")).compile.drain.start)
        _      <- Resource.liftF(out.subscribers.evalMap(l => Logger[IO].debug(s"Subscribers amount: $l")).compile.drain.start)
        f      <- Resource.liftF(engine.eventStream(inq).flatMap(SeqexecEngine.logEvent[IO]("eventStream")).through(out.publish).compile.drain.onError(logError).start)
        _      <- Resource.liftF(f.join) // We need to join to catch uncaught errors
      } yield ExitCode.Success

    seqexec.use(_ => IO.never)

  }

  /** Reads the configuration and launches the seqexec */
  override def run(args: List[String]): IO[ExitCode] =
    seqexec.guaranteeCase {
      case ExitCode.Success => IO.unit
      case e                => IO(Console.println(s"Exit code $e")) // scalastyle:off console.io
    }

}
