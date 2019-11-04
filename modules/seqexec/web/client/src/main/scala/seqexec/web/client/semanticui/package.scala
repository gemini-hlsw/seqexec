// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.CatsReact._
import react.common.Css
import react.common.implicits._
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.button.Button.LeftLabeled
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.semanticui.elements.icon.Icon

package object semanticui {

  // Custom attributes used by SemanticUI
  val dataTab: VdomAttr[String]      = VdomAttr("data-tab")
  val dataTooltip: VdomAttr[String]  = VdomAttr("data-tooltip")
  val dataContent: VdomAttr[String]  = VdomAttr("data-content")
  val dataPosition: VdomAttr[String] = VdomAttr("data-position")
  val dataInverted: VdomAttr[String] = VdomAttr("data-inverted")
  val formId: VdomAttr[String]       = VdomAttr("form")

  implicit val cssReuse: Reusability[Css] = Reusability.byEq

  def controlButton(icon:     Icon,
                    color:    String,
                    onClick:  Callback,
                    disabled: Boolean,
                    tooltip:  String,
                    text:     String): VdomElement =
    Popup("button", tooltip)(
      Button(
        icon     = Some(icon),
        labeled  = LeftLabeled,
        onClick  = onClick,
        color    = Some(color),
        disabled = disabled
      )(text)
    )
}
