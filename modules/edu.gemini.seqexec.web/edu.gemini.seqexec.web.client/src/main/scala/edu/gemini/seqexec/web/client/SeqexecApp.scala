// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client

import edu.gemini.seqexec.web.client.components.SeqexecUI
import edu.gemini.seqexec.web.client.services.log.ConsoleHandler
import edu.gemini.seqexec.web.client.services.SeqexecWebClient
import edu.gemini.seqexec.web.client.model.Pages
import edu.gemini.seqexec.web.client.actions.Initialize
import edu.gemini.seqexec.web.client.circuit.SeqexecCircuit
import edu.gemini.seqexec.model.Model.SeqexecSite
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import java.util.logging.{Level, Logger}
import java.time.ZoneId

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.OnUnmount
import japgolly.scalajs.react.extra.router.Resolution

import cats.effect.IO

/**
  * Seqexec WebApp entry point
  */
@JSExportTopLevel("seqexec.SeqexecApp")
object SeqexecApp {
  // These are unused but they bring the css from webpack
  // val semanticUICss: String = WebpackResources.SemanticUILessResource.resource
  // val seqexecCss: String = WebpackResources.SeqexecLessResource.resource
  //
  private val defaultFmt = "[%4$s] %1s - %5$s"

  def setupLogFormat: IO[String] = IO {
    // Set the global formatting for log messages
    System.setProperty("java.util.logging.SimpleFormatter.format", defaultFmt)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def setupLogger: IO[Unit] = IO {
    // Using the root logger setup the handlers
    val rootLogger = Logger.getLogger("edu")
    rootLogger.addHandler(new ConsoleHandler(Level.INFO))

    SeqexecWebClient.start()
    ()
  }

  def setupSite(site: String): IO[SeqexecSite] = IO {
    site match {
      case "GN" => SeqexecSite.SeqexecGN(ZoneId.of("Pacific/Honolulu"))
      case _    => SeqexecSite.SeqexecGS(ZoneId.of("America/Santiago"))
    }
  }

  def initializeDataModel(seqexecSite: SeqexecSite): IO[Unit] = IO {
    // Set the instruments before adding it to the dom
    SeqexecCircuit.dispatch(Initialize(seqexecSite))
  }

  def renderingNode: IO[Element] = IO {
    // Find the node where we render
    document.getElementById("content")
  }

  @JSExport
  def start(site: String): Unmounted[Unit, Resolution[Pages.SeqexecPages], OnUnmount.Backend]#Mounted = {
    // Render the UI using React
    val program = for {
      _           <- setupLogFormat
      _           <- setupLogger
      seqexecSite <- setupSite(site)
      _           <- initializeDataModel(seqexecSite)
      router      <- SeqexecUI.router(seqexecSite)
      node        <- renderingNode
    } yield router().renderIntoDOM(node)
    program.unsafeRunSync()
  }
}
