// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.effect.IO
import gem.enum.Site
import java.util.logging.{Level, Logger}
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import seqexec.web.client.components.SeqexecUI
import seqexec.web.client.services.log.ConsoleHandler
import seqexec.web.client.services.SeqexecWebClient
import seqexec.web.client.actions.{Initialize, WSClose}
import seqexec.web.client.circuit.SeqexecCircuit

/**
  * Seqexec WebApp entry point
  */
@JSExportTopLevel("seqexec.SeqexecApp")
object SeqexecApp {
  private val defaultFmt = "[%4$s] %1s - %5$s"

  def setupLogFormat: IO[String] = IO {
    // Set the global formatting for log messages
    System.setProperty("java.util.logging.SimpleFormatter.format", defaultFmt)
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def setupLogger: IO[Unit] = IO {
    // Using the root logger setup the handlers
    val rootLogger = Logger.getLogger("seqexec")
    rootLogger.addHandler(new ConsoleHandler(Level.INFO))

    SeqexecWebClient.start()
    ()
  }

  def setupSite: IO[Site] = IO.fromFuture {
    IO {
      import scala.concurrent.ExecutionContext.Implicits.global

      // Read the site from the webserver
      SeqexecWebClient.site().map(Site.fromTag(_).getOrElse(Site.GS))
    }
  }

  def initializeDataModel(seqexecSite: Site): IO[Unit] = IO {
    // Set the instruments before adding it to the dom
    SeqexecCircuit.dispatch(Initialize(seqexecSite))
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def renderingNode: IO[Element] = IO {
    // Find or create the node where we render
    Option(document.getElementById("root")).getOrElse {
      val elem = document.createElement("div")
      elem.id = "root"
      document.body.appendChild(elem)
      elem
    }
  }

  @JSExport
  def stop(): Unit =
    // Close the websocket
    SeqexecCircuit.dispatch(WSClose)

  @JSExport
  def start(): Unit =
    // Render the UI using React
    (for {
      _           <- setupLogFormat
      _           <- setupLogger
      seqexecSite <- setupSite
      _           <- initializeDataModel(seqexecSite)
      router      <- SeqexecUI.router(seqexecSite)
      node        <- renderingNode
    } yield router().renderIntoDOM(node)).unsafeRunAsync {
      case Left(e)  => e.printStackTrace
      case Right(_) => // All allright
    }

}
