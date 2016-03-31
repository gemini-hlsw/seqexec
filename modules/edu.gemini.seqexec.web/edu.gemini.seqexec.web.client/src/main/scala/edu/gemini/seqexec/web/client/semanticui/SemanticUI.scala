package edu.gemini.seqexec.web.client.semanticui

import org.querki.jquery.JQuery
import org.querki.jsext.{JSOptionBuilder, _}

import scala.scalajs.js

/**
  * Facade for the SemanticUI javascript. Note that there are extensions to JQuery
  */
object SemanticUI {

  @js.native
  trait JsVisiblityOptions extends js.Object

  object JsVisiblityOptions extends JsVisiblityOptionBuilder(noOpts)

  class JsVisiblityOptionBuilder(val dict: OptMap) extends JSOptionBuilder[JsVisiblityOptions, JsVisiblityOptionBuilder](new JsVisiblityOptionBuilder(_)) {
    def visibilityType(t: String) = jsOpt("type", t)
  }

  @js.native
  trait SemanticCommands extends JQuery {
    def visibility(o: JsVisiblityOptions): this.type = js.native

    def dropdown(): this.type = js.native

    def tab(): this.type = js.native
  }

  implicit def jq2Semantic(jq: JQuery): SemanticCommands = jq.asInstanceOf[SemanticCommands]

}