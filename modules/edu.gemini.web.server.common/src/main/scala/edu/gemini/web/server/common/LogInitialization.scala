// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.server.common

import java.io.File
import java.nio.file.Path
import org.slf4j.bridge.SLF4JBridgeHandler
import java.util.logging.LogManager

// import scala.io.{Codec, Source}
// import scalaz.syntax.bind._
import scalaz.concurrent.Task

trait AppBaseDir {
  /**
    * Calculates the base dir of the application based on the location of "this" class jar file
    * It will throw an exception if unable to find the base dir
    */
  def baseDir: Task[Path] = Task.delay {
    val clazz = this.getClass
    val fileName = clazz.getResource(s"/${clazz.getName.replace(".", System.getProperty("file.separator"))}.class").getFile

    // find the separator for the intra classpath location
    fileName.replace("file:", "").split("!").headOption.map { (f: String) =>
      // Find the location of the basedir relative to this class
      // it assumes the jar is in a lib dir under base
      val jarFile = new File(f).getParentFile
      jarFile.getParentFile.toPath
    }.getOrElse(throw new RuntimeException("Fatal! Cannot calculate the app base dir"))
  }
}

trait LogInitialization extends AppBaseDir {
  private val loggingConfigurationFileName = "logback.xml"

  def loggingConfigurationFile(baseDir: Path): Task[Path] = Task.delay {
    val confDir = baseDir.resolve("conf")

    confDir.resolve(loggingConfigurationFileName)
  }

  // Send logs from JULI (e.g. ocs) to SLF4J
  private def sendJuliToSLF4J: Task[Unit] = Task.delay {
    LogManager.getLogManager().reset()
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()
  }

  private def initializeLogFromConf: Task[Unit] =
    for {
      _ <- sendJuliToSLF4J
      b <- baseDir
      f <- loggingConfigurationFile(b)
      // _ <- makeLogDir(f._1)
    } yield
      // Load updated configuration, note the configuration is in memory and not persisted to the file
      ()
      // LogManager.getLogManager.readConfiguration(new ByteArrayInputStream(f._2.getBytes(Codec.UTF8.charSet)))

  // private def initializeLogFromClasspath: Task[Unit] = Task.delay{
  //   LogManager.getLogManager.readConfiguration(getClass.getClassLoader.getResourceAsStream(loggingConfigurationFileName))}
  //
  // private def defaultInitialization: Task[Unit] = Task.delay{
  //   LogManager.getLogManager.readConfiguration()}
  //
  /**
    * Attempts to load and set the configuration of the java.util.logging subsystem assuming
    * that a configuration will be at {{app.home}}/conf/logging.properties and will
    * try to create a dir for log files
    * It will attempt several strategies to get a usable configuration if errors are detected
    */
  def configLog: Task[Unit] = initializeLogFromConf//.or(initializeLogFromClasspath).or(defaultInitialization)
}
