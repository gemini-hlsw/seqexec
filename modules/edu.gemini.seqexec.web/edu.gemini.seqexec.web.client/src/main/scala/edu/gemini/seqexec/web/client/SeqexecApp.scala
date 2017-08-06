package edu.gemini.seqexec.web.client

import edu.gemini.seqexec.web.client.components.{SeqexecStyles, SeqexecUI}
import edu.gemini.seqexec.web.client.services.log.{AjaxHandler, ConsoleHandler}
import edu.gemini.seqexec.model.Model.SeqexecSite

import org.scalajs.dom.document
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import java.util.logging.{Level, Logger}

/**
  * Seqexec WebApp entry point
  */
@JSExportTopLevel("SeqexecApp")
object SeqexecApp {
  private val defaultFmt = "[%4$s] %1s - %5$s"

  // Set the global formatting for log messages
  System.setProperty("java.util.logging.SimpleFormatter.format", defaultFmt)

  // On JS args is always empty
  @JSExport
  def start(site: String): Unit = {
    val CssSettings = scalacss.devOrProdDefaults
    import CssSettings._
    // Using the root logger setup the handlers
    val rootLogger = Logger.getLogger("edu")
    rootLogger.addHandler(new ConsoleHandler(Level.INFO))
    rootLogger.addHandler(new AjaxHandler(Level.INFO))

    val log = Logger.getLogger("edu.gemini.seqexec.web.client.SeqexecApp")

    log.info(s"Starting Seqexec Web Client version: ${OcsBuildInfo.version}")

    // Register CSS styles
    SeqexecStyles.addToDocument()

    // Not to happy about this but the alternatives are complicated
    val seqexecSite = site match {
      case "GN" => SeqexecSite.SeqexecGN
      case "GS" => SeqexecSite.SeqexecGS
      case _    => SeqexecSite.SeqexecGS // Default to something reasonable
    }

    // Render the UI using React
    SeqexecUI.router(seqexecSite)().renderIntoDOM(document.getElementById("content"))
    ()
  }
}
