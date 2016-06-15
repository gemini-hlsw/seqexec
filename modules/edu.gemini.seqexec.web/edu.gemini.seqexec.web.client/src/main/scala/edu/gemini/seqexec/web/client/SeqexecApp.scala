package edu.gemini.seqexec.web.client

import java.time.{Instant, LocalDate}

import edu.gemini.seqexec.web.client.components.{SeqexecStyles, SeqexecUI}
import japgolly.scalajs.react.ReactDOM

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import org.scalajs.dom.document
import java.util.logging.{Level, Logger}

import edu.gemini.seqexec.web.client.services.log.{AjaxHandler, ConsoleHandler}

/**
  * Seqexec WebApp entry point
  */
@JSExport("SeqexcApp")
object SeqexecApp extends JSApp {
  private val defaultFmt = "[%4$s] %1s - %5$s"

  // Set the global formatting for log messages
  System.setProperty("java.util.logging.SimpleFormatter.format", defaultFmt)

  def main(): Unit = {
    // Using the root logger setup the handlers
    val rootLogger = Logger.getLogger("edu")
    rootLogger.addHandler(new ConsoleHandler)
    rootLogger.addHandler(new AjaxHandler(Level.INFO))

    val log = Logger.getLogger("edu.gemini.seqexec.web.client.SeqexecApp")

    log.info(s"Starting Seqexec Web Client version: ${OcsBuildInfo.version}")

    // Register CSS styles
    SeqexecStyles.addToDocument()

    // Render the UI using React
    ReactDOM.render(SeqexecUI(), document.getElementById("content"))
  }
}
