package edu.gemini.seqexec.web.server.http4s

import java.io.{ByteArrayInputStream, File}
import java.util.logging.{LogManager, Logger}

import org.http4s.server.Server
import org.http4s.server.blaze.BlazeBuilder

import scalaz._
import Scalaz._

import scala.io.{Codec, Source}

object WebServerLauncher extends App {
  val logger = Logger.getLogger(getClass.getName)

  // Try to load logging configuration
  \/.fromTryCatchNonFatal {
    val clazz = this.getClass
    val fileName = clazz.getResource(s"/${clazz.getName.replace(".", System.getProperty("file.separator"))}.class").getFile
    // find the separator for the intra classpath location
    fileName.replace("file:", "").split("!").headOption.foreach { f =>
      // Find the location of the logging configuration relative to this class
      // it assumes the jar is in a lib dir while the configuration is on "conf"
      val jarFile = new File(f).getParentFile
      val baseDir = jarFile.getParentFile.getAbsolutePath
      val confDir = new File(jarFile.getParent, "conf")
      val loggingConfig = new File(confDir, "logging.properties")
      if (loggingConfig.exists) {
        // Replace base dir
        val src = for {
          l <- Source.fromFile(loggingConfig)(Codec.UTF8).getLines
        } yield l.replace("{{basedir}}", baseDir)
        val processedFile: String = src.toList.mkString("\n")

        // Load updated configuration
        LogManager.getLogManager.readConfiguration(new ByteArrayInputStream(processedFile.getBytes(Codec.UTF8.charSet)))
      } else {
        // Last ditch attempt to read some configuration
        LogManager.getLogManager.readConfiguration(getClass.getClassLoader.getResourceAsStream("logging.properties"))
      }
    }
  }.leftMap(u => println("Cannot load logging configuration, defaults will be used"))

  // TODO improve configuration style
  val devMode = !args.contains("prod")

  def launch(port: Int):Option[Server] = {
    logger.info(s"Starting web server on port $port")
    try {
      Some(BlazeBuilder.bindHttp(port, "0.0.0.0")
        .withWebSockets(true)
        .mountService(StaticRoutes.service(devMode), "/")
        .mountService(SeqexecCommandRoutes.service, "/api/seqexec/commands")
        .mountService(SeqexecUIApiRoutes.service, "/api")
        .run)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        None
    }
  }
  launch(9090).foreach(_.awaitShutdown())
}
