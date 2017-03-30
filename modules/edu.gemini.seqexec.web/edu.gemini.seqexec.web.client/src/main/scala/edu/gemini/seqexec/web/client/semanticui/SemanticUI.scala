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
    def offset(t: Int) = jsOpt("offset", t)
  }

  @js.native
  trait JsModalOptions extends js.Object

  object JsModalOptions extends JsModalOptionBuilder(noOpts)

  class JsModalOptionBuilder(val dict: OptMap) extends JSOptionBuilder[JsModalOptions, JsModalOptionBuilder](new JsModalOptionBuilder(_)) {
    def autofocus(t: Boolean) = jsOpt("autofocus", t)
    def onDeny[A](t: js.Function0[A]) = jsOpt("onDeny", t)
    def onHide[A](t: js.Function0[A]) = jsOpt("onHide", t)
    def onHidden[A](t: js.Function0[A]) = jsOpt("onHidden", t)
    def onApprove[A](t: js.Function0[A]) = jsOpt("onApprove", t)
  }

  @js.native
  trait JsProgressOptions extends js.Object

  object JsProgressOptions extends JsProgressOptionBuilder(noOpts)

  class JsProgressOptionBuilder(val dict: OptMap) extends JSOptionBuilder[JsProgressOptions, JsProgressOptionBuilder](new JsProgressOptionBuilder(_)) {
    def total(v: Int) = jsOpt("total", v)
    def value(v: Int) = jsOpt("value", v)
  }

  @js.native
  trait JsDropdownOptions extends js.Object

  object JsDropdownOptions extends JsDropdownOptionBuilder(noOpts)

  class JsDropdownOptionBuilder(val dict: OptMap) extends JSOptionBuilder[JsDropdownOptions, JsDropdownOptionBuilder](new JsDropdownOptionBuilder(_)) {
    def onChange[A, B, C](t: js.Function2[A, B, C]) = jsOpt("onChange", t)
  }

  @js.native
  trait SemanticCommands extends JQuery {
    def visibility(o: JsVisiblityOptions): this.type = js.native

    def dropdown(): this.type = js.native
    def dropdown(cmd: String): this.type = js.native
    def dropdown(o: JsDropdownOptions): this.type = js.native

    def tab(): this.type = js.native

    def transition(s: String): this.type = js.native

    def modal(s: String): this.type = js.native

    def modal(o: JsModalOptions): this.type = js.native

    def progress(o: JsProgressOptions): this.type = js.native
  }

  implicit def jq2Semantic(jq: JQuery): SemanticCommands = jq.asInstanceOf[SemanticCommands]

}
