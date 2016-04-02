package edu.gemini.seqexec.web.cli

import org.querki.jquery.JQuery
import org.querki.jsext.{JSOptionBuilder, _}

import scala.scalajs.js

/**
  * Facade for jQuery terminal
  */
object JQueryTerminal {

  @js.native
  trait JsTerminalOptions extends js.Object

  object JsTerminalOptions extends JsTerminalOptionBuilder(noOpts)

  class JsTerminalOptionBuilder(val dict: OptMap) extends JSOptionBuilder[JsTerminalOptions, JsTerminalOptionBuilder](new JsTerminalOptionBuilder(_)) {
    def prompt(t: String) = jsOpt("prompt", t)
  }


  @js.native
    trait TerminalCommands extends JQuery {
    def terminal(o: JsTerminalOptions): this.type = js.native
  }

  implicit def jq2Semantic(jq: JQuery): TerminalCommands = jq.asInstanceOf[TerminalCommands]
}
