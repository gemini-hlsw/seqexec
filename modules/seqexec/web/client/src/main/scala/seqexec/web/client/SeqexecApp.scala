// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.effect.IO
import gem.enum.Site
import org.scalajs.dom.document
import org.scalajs.dom.raw.Element
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel
import seqexec.web.client.components.SeqexecUI
import seqexec.web.client.services.SeqexecWebClient
import seqexec.web.client.actions.Initialize
import seqexec.web.client.actions.WSClose
import seqexec.web.client.circuit.SeqexecCircuit
import org.log4s._

/**
  * Seqexec WebApp entry point
  */
@JSExportTopLevel("SeqexecApp")
object SeqexecApp {

  def setupLogger: IO[Unit] = IO {
    import Log4sConfig._
    setLoggerThreshold("seqexec", Info)
    setLoggerThreshold("", AllThreshold)
  }

  def serverSite: IO[Site] = IO.fromFuture {
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
      _           <- setupLogger
      seqexecSite <- serverSite
      _           <- initializeDataModel(seqexecSite)
      router      <- SeqexecUI.router(seqexecSite)
      node        <- renderingNode
    } yield router().renderIntoDOM(node)).unsafeRunAsync {
      case Left(e)  => e.printStackTrace
      case Right(_) => // All allright
    }

}
