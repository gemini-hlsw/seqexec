// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.facades.semanticui

import org.querki.jquery.JQuery
import org.querki.jsext.{JSOptionBuilder, _}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/**
  * Facades for the SemanticUI javascript modal
  */
object SemanticUITransition {

  @js.native
  @JSImport("semantic-ui-transition", JSImport.Default)
  private object SemanticTransitionModule extends js.Any

  SemanticTransitionModule

  @js.native
  trait SemanticTransition extends JQuery {
    def transition(s: String): this.type
  }

  implicit def jq2Semantic($: JQuery): SemanticTransition = {
    $.asInstanceOf[SemanticTransition]
  }

}

object SemanticUIModal {
  @js.native
  @JSImport("semantic-ui-transition", JSImport.Default)
  private object SemanticTransitionModule extends js.Any

  SemanticTransitionModule

  @js.native
  @JSImport("semantic-ui-dimmer", JSImport.Default)
  private object SemanticDimmerModule extends js.Any

  SemanticDimmerModule

  @js.native
  @JSImport("semantic-ui-modal", JSImport.Default)
  private object SemanticModalModule extends js.Any

  SemanticModalModule

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
  trait SemanticModal extends js.Object {
    def modal(s: String): this.type
    def modal(o: JsModalOptions): this.type
  }

  implicit def jq2Semantic(jQuery: JQuery): SemanticModal = jQuery.asInstanceOf[SemanticModal]
}
