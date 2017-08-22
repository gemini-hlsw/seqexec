// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.p1backend.server.http4s

import edu.gemini.p1backend.server.OcsBuildInfo
import edu.gemini.web.server.common.{LogInitialization, StaticRoutes}
import knobs._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.util.ProcessApp
import org.http4s.server.Server

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process

object WebServerLauncher extends ProcessApp with LogInitialization {

  /**
    * Configuration for the web server
    */
  case class WebServerConfiguration(host: String, port: Int, devMode: Boolean)

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
      val host = cfg.require[String]("web-server.host")
      val port = cfg.require[Int]("web-server.port")
      val devMode = cfg.require[String]("mode")
      WebServerConfiguration(host, port, devMode.equalsIgnoreCase("dev"))
    }

  /**
    * Configures and builds the web server
    */
  def webServer: Kleisli[Task, WebServerConfiguration, Server] = Kleisli { conf =>
    BlazeBuilder.bindHttp(conf.port, conf.host)
      .withWebSockets(true)
      .mountService(new StaticRoutes(index(conf.devMode, OcsBuildInfo.builtAtMillis), conf.devMode, OcsBuildInfo.builtAtMillis).service, "/")
      .start
  }

  /**
    * Reads the configuration and launches the web server
    */
  override def process(args: List[String]): Process[Task, Nothing] = {
    // Launch web server
    Process.eval_(for {
      _  <- configLog
      sc <- serverConf
      ws <- webServer.run(sc)
    } yield ws)
  }

}
