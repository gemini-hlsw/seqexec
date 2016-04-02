package edu.gemini.seqexec.web.cli

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

import org.querki.jquery.$
import org.scalajs.dom.document

import JQueryTerminal._

@JSExport("SeqexecTerminal")
object SeqexecTerminal extends JSApp {

  override def main(): Unit = {
    $(document.body).terminal(JsTerminalOptions.prompt("seqexec >"))
  }
}
