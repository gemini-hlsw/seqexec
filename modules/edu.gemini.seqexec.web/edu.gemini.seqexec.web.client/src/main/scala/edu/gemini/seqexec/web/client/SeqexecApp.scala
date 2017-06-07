package edu.gemini.seqexec.web.client

import edu.gemini.seqexec.web.client.components.{SeqexecStyles, SeqexecUI}
import japgolly.scalajs.react.extra.router.{Redirect, Router, RouterConfigDsl, BaseUrl}
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.vdom.html_<^._

import scala.scalajs.js.JSApp
import org.scalajs.dom.document
import java.util.logging.{Level, Logger}

import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, WSConnect}
import edu.gemini.seqexec.web.client.services.log.{AjaxHandler, ConsoleHandler}

/**
  * Seqexec WebApp entry point
  */
object SeqexecApp extends JSApp {
  private val defaultFmt = "[%4$s] %1s - %5$s"

  // Set the global formatting for log messages
  System.setProperty("java.util.logging.SimpleFormatter.format", defaultFmt)

  def main(): Unit = {
    sealed trait SeqexecPages
    case object Root extends SeqexecPages
    case class Instrument(name: String) extends SeqexecPages

    val routerConfig = RouterConfigDsl[SeqexecPages].buildConfig { dsl =>
      import dsl._
      // Static routes

      (emptyRule
      | staticRoute(root,     Root)  ~> render(SeqexecUI.component())
    ) .notFound(redirectToPage(Root)(Redirect.Replace)).logToConsole
    }

    val router = Router(BaseUrl.fromWindowOrigin, routerConfig)

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

    // Initiate the WebSocket connection
    SeqexecCircuit.dispatch(WSConnect(0))

    // Render the UI using React
    router().renderIntoDOM(document.getElementById("content"))
    //SeqexecUI().renderIntoDOM(document.getElementById("content"))
  }
}
