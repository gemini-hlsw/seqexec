package edu.gemini.web.server.common

import java.io.{ByteArrayInputStream, File}
import java.util.logging.LogManager

import scala.io.{Codec, Source}
import scalaz.syntax.bind._
import scalaz.syntax.std.boolean._
import scalaz.concurrent.Task

trait AppBaseDir {
  /**
    * Calculates the base dir of the application based on the location of "this" class jar file
    * It will throw an exception if unable to find the base dir
    */
  def baseDir: Task[File] = Task.delay {
    val clazz = this.getClass
    val fileName = clazz.getResource(s"/${clazz.getName.replace(".", System.getProperty("file.separator"))}.class").getFile

    // find the separator for the intra classpath location
    fileName.replace("file:", "").split("!").headOption.map { (f: String) =>
      // Find the location of the basedir relative to this class
      // it assumes the jar is in a lib dir under base
      val jarFile = new File(f).getParentFile
      jarFile.getParentFile
    }.getOrElse(throw new RuntimeException("Fatal! Cannot calculate the app base dir"))
  }
}

trait LogInitialization extends AppBaseDir {
  private val loggingConfigurationFileName = "logging.properties"

  def loggingConfigurationFile(baseDir: File): Task[File] = Task.delay {
    val confDir = new File(baseDir, "conf")

    new File(confDir, loggingConfigurationFileName)
  }

  private def readConf(baseDir: File): Task[(File, String)] = {
    def replaceLogDir(loggingConfig: File): Task[(File, String)] = Task.delay {
        loggingConfig.exists.fold({
          val logDir = new File(baseDir, "log")
          // Replace base dir
          val src = for {
            l <- Source.fromFile(loggingConfig)(Codec.UTF8).getLines
          } yield l.replace("{{log.dir}}", logDir.getAbsolutePath)

          (logDir, src.toList.mkString("\n"))
        }, throw new RuntimeException("Cannot read the logging configuration file"))
      }

    loggingConfigurationFile(baseDir) >>= replaceLogDir
  }

  private def makeLogDir(logDir: File): Task[Unit] = Task.delay {
    (logDir.exists() || logDir.mkdirs()).fold((), throw new RuntimeException(s"Cannot create logging dir $logDir"))
  }

  private def initializeLogFromConf: Task[Unit] =
    for {
      b <- baseDir
      f <- readConf(b)
      _ <- makeLogDir(f._1)
    } yield {
      // Load updated configuration, note the configuration is in memory and not persisted to the file
      LogManager.getLogManager.readConfiguration(new ByteArrayInputStream(f._2.getBytes(Codec.UTF8.charSet)))
    }

  private def initializeLogFromClasspath: Task[Unit] = Task.delay(
    LogManager.getLogManager.readConfiguration(getClass.getClassLoader.getResourceAsStream(loggingConfigurationFileName)))

  private def defaultInitialization: Task[Unit] = Task.delay(
    LogManager.getLogManager.readConfiguration())

  /**
    * Attempts to load and set the configuration of the java.util.logging subsystem assuming
    * that a configuration will be at {{app.home}}/conf/logging.properties and will
    * try to create a dir for log files
    * It will attempt several strategies to get a usable configuration if errors are detected
    */
  def configLog: Task[Unit] = initializeLogFromConf.or(initializeLogFromClasspath).or(defaultInitialization)
}
