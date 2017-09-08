// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client

import edu.gemini.seqexec.web.client.components.{SeqexecStyles, SeqexecUI}
import edu.gemini.seqexec.web.client.services.log.{AjaxHandler, ConsoleHandler}
import edu.gemini.seqexec.web.client.model.Pages
import edu.gemini.seqexec.web.client.actions.Initialize
import edu.gemini.seqexec.web.client.circuit.SeqexecCircuit
import edu.gemini.seqexec.model.Model.SeqexecSite
import org.scalajs.dom.document

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import java.util.logging.{Level, Logger}

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react.extra.router.Resolution

/**
  * Seqexec WebApp entry point
  */
@JSExportTopLevel("SeqexecApp")
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object SeqexecApp {
  private val defaultFmt = "[%4$s] %1s - %5$s"


  // On JS args is always empty
  @JSExport
  def start(site: String): Unmounted[Unit, Resolution[Pages.SeqexecPages], OnUnmount.Backend]#Mounted = {
    // Set the global formatting for log messages
    System.setProperty("java.util.logging.SimpleFormatter.format", defaultFmt)
    // Using the root logger setup the handlers
    val rootLogger = Logger.getLogger("edu")
    rootLogger.addHandler(new ConsoleHandler(Level.INFO))
    rootLogger.addHandler(new AjaxHandler(Level.INFO))

    val log = Logger.getLogger("edu.gemini.seqexec.web.client.SeqexecApp")
    log.info(s"Starting Seqexec Web Client version: ${OcsBuildInfo.version}")

    val CssSettings = scalacss.devOrProdDefaults
    import CssSettings._
    // Register CSS styles
    SeqexecStyles.addToDocument()

    // Not to happy about this but the alternatives are complicated
    val seqexecSite = site match {
      case "GN" => SeqexecSite.SeqexecGN
      case "GS" => SeqexecSite.SeqexecGS
      case _    => SeqexecSite.SeqexecGS // Default to something reasonable
    }

    // Set the instruments before adding it to the dom
    SeqexecCircuit.dispatch(Initialize(seqexecSite))

    // Render the UI using React
    SeqexecUI.router(seqexecSite)().renderIntoDOM(document.getElementById("content"))
  }
}
