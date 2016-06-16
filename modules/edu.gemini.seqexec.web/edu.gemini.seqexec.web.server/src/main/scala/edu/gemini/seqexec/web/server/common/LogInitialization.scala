package edu.gemini.seqexec.web.server.common

import java.io.{ByteArrayInputStream, File}
import java.util.logging.LogManager

import scala.io.{Codec, Source}
import scalaz.\/

trait LogInitialization {
  // Try to load logging configuration
  \/.fromTryCatchNonFatal {
    val clazz = this.getClass
    val fileName = clazz.getResource(s"/${clazz.getName.replace(".", System.getProperty("file.separator"))}.class").getFile
    // find the separator for the intra classpath location
    fileName.replace("file:", "").split("!").headOption.foreach { f =>
      // Find the location of the logging configuration relative to this class
      // it assumes the jar is in a lib dir while the configuration is on "conf"
      val jarFile = new File(f).getParentFile
      val baseDir = jarFile.getParentFile
      val confDir = new File(jarFile.getParent, "conf")
      val loggingConfig = new File(confDir, "logging.properties")
      if (loggingConfig.exists) {
        val logDir = new File(baseDir, "log")
        // Replace base dir
        val src = for {
          l <- Source.fromFile(loggingConfig)(Codec.UTF8).getLines
        } yield l.replace("{{log.dir}}", logDir.getAbsolutePath)

        if (!(logDir.exists() || logDir.mkdirs())) {
          println(s"Error creating the log dir at ${logDir.getAbsolutePath}")
          // Fatal error, exit immediately
          sys.exit(1)
        }
        val processedFile: String = src.toList.mkString("\n")

        // Load updated configuration, note the configuration is in memory and not persisted to the file
        LogManager.getLogManager.readConfiguration(new ByteArrayInputStream(processedFile.getBytes(Codec.UTF8.charSet)))
      } else {
        // Last ditch attempt to read some configuration
        LogManager.getLogManager.readConfiguration(getClass.getClassLoader.getResourceAsStream("logging.properties"))
      }
    }
  }.leftMap { u =>
    println("Cannot load logging configuration, defaults will be used")
    LogManager.getLogManager.readConfiguration()
  }

}
