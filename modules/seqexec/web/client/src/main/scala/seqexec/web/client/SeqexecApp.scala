// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.effect.Sync
import cats.effect._
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
import scala.concurrent.ExecutionContext

/**
  * Seqexec WebApp entry point
  */
final class SeqexecLauncher[F[_]](implicit val F: Sync[F], L: LiftIO[F]) {

  def setupLogger: F[Unit] = F.delay {
    import Log4sConfig._
    setLoggerThreshold("seqexec", Info)
    setLoggerThreshold("", AllThreshold)
  }

  def serverSite(implicit cs: ContextShift[IO]): F[Site] =
    L.liftIO(IO.fromFuture {
      IO {
        import ExecutionContext.Implicits.global

        // Read the site from the webserver
        SeqexecWebClient.site().map(Site.fromTag(_).getOrElse(Site.GS))
      }
    })

  def initializeDataModel(seqexecSite: Site): F[Unit] = F.delay {
    // Set the instruments before adding it to the dom
    SeqexecCircuit.dispatch(Initialize(seqexecSite))
  }

  def renderingNode: F[Element] = F.delay {
    // Find or create the node where we render
    Option(document.getElementById("root")).getOrElse {
      val elem = document.createElement("div")
      elem.id = "root"
      document.body.appendChild(elem)
      elem
    }
  }
}

/**
  * Seqexec WebApp entry point
  * Exposed to the js world
  */
@JSExportTopLevel("SeqexecApp")
object SeqexecApp extends IOApp {
  override def run(args:  List[String]): IO[ExitCode] = {
    val launcher = new SeqexecLauncher[IO]
    // Render the UI using React
    for {
      _           <- launcher.setupLogger
      seqexecSite <- launcher.serverSite
      _           <- launcher.initializeDataModel(seqexecSite)
      router      <- SeqexecUI.router[IO](seqexecSite)
      node        <- launcher.renderingNode
      _           <- IO(router().renderIntoDOM(node)).handleErrorWith(p => IO(getLogger.error(p.toString)))
    } yield ExitCode.Success
  }

  @JSExport
  def stop(): Unit =
    // Close the websocket
    SeqexecCircuit.dispatch(WSClose)

  @JSExport
  def start(): Unit =
    super.main(Array())

}
