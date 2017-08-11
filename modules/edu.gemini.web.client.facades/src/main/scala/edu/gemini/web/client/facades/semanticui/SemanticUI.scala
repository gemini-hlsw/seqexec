// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.client.facades.semanticui

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
    def visibilityType(t: String): JsVisiblityOptionBuilder = jsOpt("type", t)
    def offset(t: Int): JsVisiblityOptionBuilder = jsOpt("offset", t)
  }

  @js.native
  trait JsModalOptions extends js.Object

  object JsModalOptions extends JsModalOptionBuilder(noOpts)

  class JsModalOptionBuilder(val dict: OptMap) extends JSOptionBuilder[JsModalOptions, JsModalOptionBuilder](new JsModalOptionBuilder(_)) {
    def autofocus(t: Boolean): JsModalOptionBuilder = jsOpt("autofocus", t)
    def onDeny[A](t: js.Function0[A]): JsModalOptionBuilder = jsOpt("onDeny", t)
    def onHide[A](t: js.Function0[A]): JsModalOptionBuilder = jsOpt("onHide", t)
    def onHidden[A](t: js.Function0[A]): JsModalOptionBuilder = jsOpt("onHidden", t)
    def onApprove[A](t: js.Function0[A]): JsModalOptionBuilder = jsOpt("onApprove", t)
  }

  @js.native
  trait JsProgressOptions extends js.Object

  object JsProgressOptions extends JsProgressOptionBuilder(noOpts)

  class JsProgressOptionBuilder(val dict: OptMap) extends JSOptionBuilder[JsProgressOptions, JsProgressOptionBuilder](new JsProgressOptionBuilder(_)) {
    def total(v: Int): JsProgressOptionBuilder = jsOpt("total", v)
    def value(v: Int): JsProgressOptionBuilder = jsOpt("value", v)
  }

  @js.native
  trait JsDropdownOptions extends js.Object

  object JsDropdownOptions extends JsDropdownOptionBuilder(noOpts)

  class JsDropdownOptionBuilder(val dict: OptMap) extends JSOptionBuilder[JsDropdownOptions, JsDropdownOptionBuilder](new JsDropdownOptionBuilder(_)) {
    def onChange[A, B, C](t: js.Function2[A, B, C]): JsDropdownOptionBuilder = jsOpt("onChange", t)
  }

  @js.native
  trait JsTabOptions extends js.Object

  object JsTabOptions extends JsTabOptionBuilder(noOpts)

  class JsTabOptionBuilder(val dict: OptMap) extends JSOptionBuilder[JsTabOptions, JsTabOptionBuilder](new JsTabOptionBuilder(_)) {
    def onVisible[A, B](t: js.Function1[A, B]): JsTabOptionBuilder = jsOpt("onVisible", t)
  }

  @js.native
  trait SemanticCommands extends JQuery {
    def visibility(o: JsVisiblityOptions): this.type = js.native

    def dropdown(): this.type = js.native
    def dropdown(cmd: String): this.type = js.native
    def dropdown(o: JsDropdownOptions): this.type = js.native

    def tab(o: JsTabOptions): this.type = js.native

    def transition(s: String): this.type = js.native

    def modal(s: String): this.type = js.native

    def modal(o: JsModalOptions): this.type = js.native

    def progress(o: JsProgressOptions): this.type = js.native
  }

  implicit def jq2Semantic(jq: JQuery): SemanticCommands = jq.asInstanceOf[SemanticCommands]

}
