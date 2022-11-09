// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import java.io.FileInputStream
import java.nio.file.{ Path => FilePath }
import java.security.KeyStore
import java.security.Security
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext
import javax.net.ssl.TrustManagerFactory
import scala.concurrent.duration._
import cats.effect._
import cats.effect.syntax.all._
import cats.syntax.all._
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.Appender
import fs2.Stream
import cats.effect.std.{ Dispatcher, Queue }
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import io.prometheus.client.CollectorRegistry
import org.http4s.HttpRoutes
import org.http4s.client.Client
import org.http4s.client.middleware.RequestLogger
import org.http4s.client.middleware.ResponseLogger
import org.http4s.metrics.prometheus.Prometheus
import org.http4s.metrics.prometheus.PrometheusExportService
import org.http4s.server.Router
import org.http4s.server.SSLKeyStoreSupport.StoreInfo
import org.http4s.server.Server
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.server.middleware.Metrics
import org.http4s.server.middleware.{ Logger => Http4sLogger }
import pureconfig._
import seqexec.model.config._
import seqexec.model.events._
import seqexec.server
import seqexec.server.CaServiceInit
import seqexec.server.SeqexecEngine
import seqexec.server.SeqexecFailure
import seqexec.server.SeqexecMetrics
import seqexec.server.Systems
import seqexec.server.executeEngine
import seqexec.server.tcs.GuideConfigDb
import seqexec.web.server.OcsBuildInfo
import seqexec.web.server.config._
import seqexec.web.server.logging.AppenderForClients
import seqexec.web.server.security.AuthenticationService
import web.server.common.LogInitialization
import web.server.common.RedirectToHttpsRoutes
import web.server.common.StaticRoutes
import cats.effect.{ Ref, Resource, Temporal }
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.blaze.server.BlazeServerBuilder

object WebServerLauncher extends IOApp with LogInitialization {
  private implicit def L: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("seqexec")

  // Attempt to get the configuration file relative to the base dir
  def configurationFile[F[_]: Sync]: F[FilePath] =
    baseDir[F].map(_.resolve("conf").resolve("app.conf"))

  // Try to load config from the file and fall back to the common one in the class path
  def config[F[_]: Sync]: F[ConfigObjectSource] = {
    val defaultConfig = ConfigSource.resources("app.conf").pure[F]
    val fileConfig    = configurationFile.map(ConfigSource.file)

    // ConfigSource, first attempt the file or default to the classpath file
    (fileConfig, defaultConfig).mapN(_.optional.withFallback(_))
  }

  /** Configures the Authentication service */
  def authService[F[_]: Sync: Logger](
    mode: Mode,
    conf: AuthenticationConfig
  ): F[AuthenticationService[F]] =
    Sync[F].delay(AuthenticationService[F](mode, conf))

  def makeContext[F[_]: Sync](tls: TLSConfig): F[SSLContext] = Sync[F].delay {
    val ksStream   = new FileInputStream(tls.keyStore.toFile.getAbsolutePath)
    val ks         = KeyStore.getInstance("JKS")
    ks.load(ksStream, tls.keyStorePwd.toCharArray)
    ksStream.close()
    val trustStore = StoreInfo(tls.keyStore.toFile.getAbsolutePath, tls.keyStorePwd)

    val tmf = {
      val ksStream = new FileInputStream(trustStore.path)

      val ks = KeyStore.getInstance("JKS")
      ks.load(ksStream, tls.keyStorePwd.toCharArray)
      ksStream.close()

      val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)

      tmf.init(ks)
      tmf.getTrustManagers
    }

    val kmf = KeyManagerFactory.getInstance(
      Option(Security.getProperty("ssl.KeyManagerFactory.algorithm"))
        .getOrElse(KeyManagerFactory.getDefaultAlgorithm)
    )

    kmf.init(ks, tls.certPwd.toCharArray)

    val context = SSLContext.getInstance("TLS")
    context.init(kmf.getKeyManagers, tmf, null)
    context
  }

  /** Resource that yields the running web server */
  def webServer[F[_]: Logger: Async](
    conf:      SeqexecConfiguration,
    cal:       SmartGcal,
    as:        AuthenticationService[F],
    inputs:    server.EventQueue[F],
    outputs:   Topic[F, SeqexecEvent],
    se:        SeqexecEngine[F],
    cr:        CollectorRegistry,
    clientsDb: ClientsSetDb[F]
  ): Resource[F, Server] = {

    // The prometheus route does not get logged
    val prRouter = Router[F](
      "/" -> PrometheusExportService[F](cr).routes
    )

    val ssl: F[Option[SSLContext]] = conf.webServer.tls.map(makeContext[F]).sequence

    def build(all: WebSocketBuilder2[F] => HttpRoutes[F]): Resource[F, Server] =
      Resource.eval {
        val builder =
          BlazeServerBuilder[F]
            .bindHttp(conf.webServer.port, conf.webServer.host)
            .withHttpWebSocketApp(wsb => (prRouter <+> all(wsb)).orNotFound)
        ssl.map(_.fold(builder)(builder.withSslContext)).map(_.resource)
      }.flatten

    def router(wsb: WebSocketBuilder2[F]) = Router[F](
      "/"                     -> new StaticRoutes(conf.mode === Mode.Development, OcsBuildInfo.builtAtMillis).service,
      "/api/seqexec/commands" -> new SeqexecCommandRoutes(as, inputs, se).service,
      "/api"                  -> new SeqexecUIApiRoutes(conf.site,
                                       conf.mode,
                                       as,
                                       se.systems.guideDb,
                                       se.systems.gpi.statusDb,
                                       clientsDb,
                                       outputs,
                                       wsb
      ).service,
      "/api/seqexec/guide"    -> new GuideConfigDbRoutes(se.systems.guideDb).service,
      "/smartgcal"            -> new SmartGcalRoutes[F](cal).service
    )

    val pingRouter = Router[F](
      "/ping" -> new PingRoutes(as).service
    )

    def loggedRoutes(wsb: WebSocketBuilder2[F])                               =
      pingRouter <+> Http4sLogger.httpRoutes(logHeaders = false, logBody = false)(router(wsb))
    val metricsMiddleware: Resource[F, WebSocketBuilder2[F] => HttpRoutes[F]] =
      Prometheus.metricsOps[F](cr, "seqexec").map(x => wsb => Metrics[F](x)(loggedRoutes(wsb)))

    metricsMiddleware.flatMap(x => build(x))

  }

  def redirectWebServer[F[_]: Logger: Async](
    gcdb: GuideConfigDb[F],
    cal:  SmartGcal
  )(conf: WebServerConfiguration): Resource[F, Server] = {
    val router = Router[F](
      "/api/seqexec/guide" -> new GuideConfigDbRoutes(gcdb).service,
      "/smartgcal"         -> new SmartGcalRoutes[F](cal).service,
      "/"                  -> new RedirectToHttpsRoutes[F](443, conf.externalBaseUrl).service
    )

    BlazeServerBuilder[F]
      .bindHttp(conf.insecurePort, conf.host)
      .withHttpApp(router.orNotFound)
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
    val msg    =
      s"""Start web server for site ${conf.site} on ${conf.mode} mode, version ${OcsBuildInfo.version}"""
    Logger[F].info(banner + msg)
  }

  // We need to manually update the configuration of the logging subsystem
  // to support capturing log messages and forward them to the clients
  def logToClients(
    out:        Topic[IO, SeqexecEvent],
    dispatcher: Dispatcher[IO]
  ): IO[Appender[ILoggingEvent]] = IO.apply {
    import ch.qos.logback.classic.{ AsyncAppender, Logger, LoggerContext }
    import org.slf4j.LoggerFactory

    val asyncAppender = new AsyncAppender
    val appender      = new AppenderForClients(out)(dispatcher)
    Option(LoggerFactory.getILoggerFactory)
      .collect { case lc: LoggerContext =>
        lc
      }
      .foreach { ctx =>
        asyncAppender.setContext(ctx)
        appender.setContext(ctx)
        asyncAppender.addAppender(appender)
      }

    Option(LoggerFactory.getLogger("seqexec"))
      .collect { case l: Logger =>
        l
      }
      .foreach { l =>
        l.addAppender(asyncAppender)
        asyncAppender.start()
        appender.start()
      }
    asyncAppender
  }

  // Logger of error of last resort.
  def logError[F[_]: Logger]: PartialFunction[Throwable, F[Unit]] = {
    case e: SeqexecFailure =>
      Logger[F].error(e)(s"Seqexec global error handler ${SeqexecFailure.explain(e)}")
    case e: Exception      => Logger[F].error(e)("Seqexec global error handler")
  }

  /** Reads the configuration and launches the seqexec engine and web server */
  def seqexec: IO[ExitCode] = {

    // Override the default client config
    def mkClient(timeout: FiniteDuration): Resource[IO, Client[IO]] =
      JdkHttpClient.simple[IO].map(c => Client(r => c.run(r).timeout(timeout)))

    def engineIO(
      conf:       SeqexecConfiguration,
      httpClient: Client[IO],
      collector:  CollectorRegistry
    ): Resource[IO, SeqexecEngine[IO]] =
      for {
        met  <- Resource.eval(SeqexecMetrics.build[IO](conf.site, collector))
        caS  <- Resource.eval(CaServiceInit.caInit[IO](conf.seqexecEngine))
        sys  <- Systems.build(conf.site, httpClient, conf.seqexecEngine, caS)
        seqE <- Resource.eval(SeqexecEngine.build(conf.site, sys, conf.seqexecEngine, met))
      } yield seqE

    def webServerIO(
      conf: SeqexecConfiguration,
      in:   Queue[IO, executeEngine.EventType],
      out:  Topic[IO, SeqexecEvent],
      en:   SeqexecEngine[IO],
      cr:   CollectorRegistry,
      cs:   ClientsSetDb[IO]
    ): Resource[IO, Unit] =
      for {
        as <- Resource.eval(authService[IO](conf.mode, conf.authentication))
        ca <- Resource.eval(SmartGcalInitializer.init[IO](conf.smartGcal))
        _  <- redirectWebServer(en.systems.guideDb, ca)(conf.webServer)
        _  <- webServer[IO](conf, ca, as, in, out, en, cr, cs)
      } yield ()

    def publishStats[F[_]: Temporal](cs: ClientsSetDb[F]): Stream[F, Unit] =
      Stream.fixedRate[F](10.minute).flatMap(_ => Stream.eval(cs.report))

    val seqexec: Resource[IO, ExitCode] =
      for {
        _      <- Resource.eval(configLog[IO]) // Initialize log before the engine is setup
        conf   <- Resource.eval(config[IO].flatMap(loadConfiguration[IO]))
        _      <- Resource.eval(printBanner(conf))
        cli    <- mkClient(conf.seqexecEngine.dhsTimeout)
                    .map(RequestLogger(true, true))
                    .map(ResponseLogger(true, true))
        inq    <- Resource.eval(Queue.bounded[IO, executeEngine.EventType](10))
        out    <- Resource.eval(Topic[IO, SeqexecEvent])
        dsp    <- Dispatcher[IO]
        _      <- Resource.eval(logToClients(out, dsp))
        cr     <- Resource.eval(IO(new CollectorRegistry))
        cs     <- Resource.eval(
                    Ref.of[IO, ClientsSetDb.ClientsSet](Map.empty).map(ClientsSetDb.apply[IO](_))
                  )
        _      <- Resource.eval(publishStats(cs).compile.drain.start)
        engine <- engineIO(conf, cli, cr)
        _      <- webServerIO(conf, inq, out, engine, cr, cs)
        _      <- Resource.eval(
                    inq.size
                      .map(l => Logger[IO].debug(s"Queue length: $l").whenA(l > 1))
                      .start
                  )
        _      <- Resource.eval(
                    out.subscribers
                      .evalMap(l => Logger[IO].debug(s"Subscribers amount: $l").whenA(l > 1))
                      .compile
                      .drain
                      .start
                  )
        f      <- Resource.eval(
                    engine.eventStream(inq).through(out.publish).compile.drain.onError(logError).start
                  )
        _      <- Resource.eval(f.join)        // We need to join to catch uncaught errors
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
