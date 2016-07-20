package edu.gemini.seqexec.web.server.common

import java.io.{ByteArrayInputStream, File}
import java.util.logging.LogManager

import scala.io.{Codec, Source}
import scalaz._
import Scalaz._
import scalaz.effect.IO

trait LogInitialization {
  sealed trait LogConfError
  case class ConfigurationNotFound(s: String) extends LogConfError
  case class LogDirNotFound(f: File) extends LogConfError
  case object ConfigurationLoadingError extends LogConfError

  type LogConf[A] = EitherT[IO, LogConfError, A]

  object LogConf {
    def fromDisjunction[A](a: LogConfError \/ A): LogConf[A] = EitherT(IO(a))
    def fromConfLoading[A](a: => A): LogConf[A] =
      fromDisjunction(\/.fromTryCatchNonFatal(a).leftMap(_ => ConfigurationLoadingError))
    val unit: LogConf[Unit] = ().point[LogConf]
  }

  private def readConf: LogConf[(File, String)] = EitherT {
    IO {
      val clazz = this.getClass
      val fileName = clazz.getResource(s"/${clazz.getName.replace(".", System.getProperty("file.separator"))}.class").getFile

      // find the separator for the intra classpath location
      fileName.replace("file:", "").split("!").headOption.map { (f: String) =>
        // Find the location of the logging configuration relative to this class
        // it assumes the jar is in a lib dir while the configuration is on "conf"
        val jarFile = new File(f).getParentFile
        val baseDir = jarFile.getParentFile
        val confDir = new File(jarFile.getParent, "conf")
        val loggingConfig = new File(confDir, "logging.properties")
        loggingConfig.exists.fold({
          val logDir = new File(baseDir, "log")
          // Replace base dir
          val src = for {
            l <- Source.fromFile(loggingConfig)(Codec.UTF8).getLines
          } yield l.replace("{{log.dir}}", logDir.getAbsolutePath)

          \/-((logDir, src.toList.mkString("\n")))
        }, -\/(ConfigurationNotFound(fileName)))
      }.getOrElse(-\/(ConfigurationNotFound(fileName)))
    }
  }

  private def makeLogDir(logDir: File): LogConf[Unit] = EitherT {
    IO {
      (logDir.exists() || logDir.mkdirs()).fold(\/-(()), -\/(LogDirNotFound(logDir)))
    }
  }

  private def initializeLogFromConf(conf: String): LogConf[Unit] = LogConf.fromConfLoading(
    // Load updated configuration, note the configuration is in memory and not persisted to the file
    LogManager.getLogManager.readConfiguration(new ByteArrayInputStream(conf.getBytes(Codec.UTF8.charSet))))

  private def initializeLogFromClasspath: LogConf[Unit] = LogConf.fromConfLoading(
    LogManager.getLogManager.readConfiguration(getClass.getClassLoader.getResourceAsStream("logging.properties")))

  private def defaultInitialization: LogConf[Unit] = LogConf.fromConfLoading(
    LogManager.getLogManager.readConfiguration())

  private def loadConfiguration(conf: String): LogConf[Unit] =
    initializeLogFromConf(conf).orElse(initializeLogFromClasspath).orElse(defaultInitialization)

  def configLog: LogConf[Unit] = {
    for {
      f <- readConf
      _ <- makeLogDir(f._1)
      u <- loadConfiguration(f._2)
    } yield u
  }
}
