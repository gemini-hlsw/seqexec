// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.facades.semanticui

import org.querki.jquery.JQuery
import org.querki.jsext.JSOptionBuilder
import org.querki.jsext._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/**
  * Facades for the SemanticUI javascript. Note that there are extensions to JQuery
  */
object SemanticUIDropDown {

  @js.native
  @JSImport("semantic-ui-dropdown", JSImport.Default)
  private object SemanticDropDownModule extends js.Any

  SemanticDropDownModule

  @js.native
  trait JsDropdownOptions extends js.Object

  object JsDropdownOptions extends JsDropdownOptionBuilder(noOpts)

  class JsDropdownOptionBuilder(val dict: OptMap)
      extends JSOptionBuilder[JsDropdownOptions, JsDropdownOptionBuilder](
        new JsDropdownOptionBuilder(_)) {
    def onChange[A, B, C](t: js.Function2[A, B, C]): JsDropdownOptionBuilder =
      jsOpt("onChange", t)
  }

  @js.native
  trait SemanticDropDown extends JQuery {
    def dropdown(): this.type = js.native
    def dropdown(cmd: String): this.type            = js.native
    def dropdown(o:   JsDropdownOptions): this.type = js.native
  }

  implicit def jq2Semantic($ : JQuery): SemanticDropDown =
    $.asInstanceOf[SemanticDropDown]

}

object SemanticUITab {

  @js.native
  @JSImport("semantic-ui-tab", JSImport.Default)
  private object SemanticTabModule extends js.Any

  SemanticTabModule

  @js.native
  trait JsTabOptions extends js.Object

  object JsTabOptions extends JsTabOptionBuilder(noOpts)

  class JsTabOptionBuilder(val dict: OptMap)
      extends JSOptionBuilder[JsTabOptions, JsTabOptionBuilder](
        new JsTabOptionBuilder(_)) {
    def onVisible[A, B](t: js.Function1[A, B]): JsTabOptionBuilder =
      jsOpt("onVisible", t)
  }

  @js.native
  trait SemanticTab extends JQuery {
    def tab(o: JsTabOptions): this.type
  }

  implicit def jq2Semantic($ : JQuery): SemanticTab =
    $.asInstanceOf[SemanticTab]

}

object SemanticUIVisibility {

  @js.native
  @JSImport("semantic-ui-visibility", JSImport.Namespace)
  private object SemanticVisibilityModule extends js.Any

  SemanticVisibilityModule

  @js.native
  trait JsVisiblityOptions extends js.Object

  object JsVisiblityOptions extends JsVisiblityOptionBuilder(noOpts)

  class JsVisiblityOptionBuilder(val dict: OptMap)
      extends JSOptionBuilder[JsVisiblityOptions, JsVisiblityOptionBuilder](
        new JsVisiblityOptionBuilder(_)) {
    def visibilityType(t: String): JsVisiblityOptionBuilder = jsOpt("type", t)
    def offset(t:         Int): JsVisiblityOptionBuilder    = jsOpt("offset", t)
  }

  @js.native
  trait SemanticVisibility extends JQuery {
    def visibility(o: JsVisiblityOptions): this.type
  }

  implicit def jq2Semantic($ : JQuery): SemanticVisibility =
    $.asInstanceOf[SemanticVisibility]

}

object SemanticUIProgress {

  @js.native
  @JSImport("semantic-ui-progress", JSImport.Namespace)
  private object SemanticProgressModule extends js.Any

  SemanticProgressModule

  @js.native
  trait JsProgressOptions extends js.Object

  object JsProgressOptions extends JsProgressOptionBuilder(noOpts)

  class JsProgressOptionBuilder(val dict: OptMap)
      extends JSOptionBuilder[JsProgressOptions, JsProgressOptionBuilder](
        new JsProgressOptionBuilder(_)) {
    def total(v:     Long): JsProgressOptionBuilder    = jsOpt("total", v)
    def value(v:     Long): JsProgressOptionBuilder    = jsOpt("value", v)
    def percent(v:   Double): JsProgressOptionBuilder  = jsOpt("percent", v)
    def debug(v:     Boolean): JsProgressOptionBuilder = jsOpt("debug", v)
    def precision(v: Int): JsProgressOptionBuilder     = jsOpt("precision", v)
    def onChange[A](
      t: js.Function3[js.Any, js.Any, js.Any, A]): JsProgressOptionBuilder =
      jsOpt("onChange", t)
  }

  @js.native
  trait SemanticProgress extends JQuery {
    def progress(o: JsProgressOptions): this.type
  }

  implicit def jq2Semantic($ : JQuery): SemanticProgress =
    $.asInstanceOf[SemanticProgress]

}

object SemanticUIPopup {

  @js.native
  @JSImport("semantic-ui-popup", JSImport.Namespace)
  private object SemanticPopupModule extends js.Any

  SemanticPopupModule

  @js.native
  trait JsPopupOptions extends js.Object

  object JsPopupOptions extends JsPopupOptionBuilder(noOpts)

  class JsPopupOptionBuilder(val dict: OptMap)
      extends JSOptionBuilder[JsPopupOptions, JsPopupOptionBuilder](
        new JsPopupOptionBuilder(_)) {
    def on(v:      String): JsPopupOptionBuilder = jsOpt("on", v)
    def title(v:   String): JsPopupOptionBuilder = jsOpt("title", v)
    def content(v: String): JsPopupOptionBuilder = jsOpt("content", v)
  }

  @js.native
  trait SemanticPopup extends JQuery {
    def popup(o: JsPopupOptions): this.type
  }

  implicit def jq2Semantic($ : JQuery): SemanticPopup =
    $.asInstanceOf[SemanticPopup]

}
