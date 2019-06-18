// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import seqexec.web.client.circuit.SequenceInfoFocus
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.elements.icon.Icon.IconCheckmark
import seqexec.web.client.semanticui.Size
import seqexec.model.SequenceState
import seqexec.model.UnknownTargetName
import react.common.implicits._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import cats.implicits._

/**
  * Display the name of the sequence and the observer
  */
object SequenceInfo {
  final case class Props(p: SequenceInfoFocus)

  private def component =
    ScalaComponent
      .builder[Props]("SequenceInfo")
      .stateless
      .render_P { p =>
        val SequenceInfoFocus(isLogged, obsName, status, tName) = p.p
        val unknownTargetName: TagMod =
          Label(Label.Props(UnknownTargetName, basic = true))
        val targetName = tName
          .filter(_.nonEmpty)
          .fold(unknownTargetName)(t => Label(Label.Props(t, basic = true)))
        <.div(
          ^.cls := "ui form",
          <.div(
            ^.cls := "fields",
            SeqexecStyles.fieldsNoBottom,
            <.div(
              ^.cls := "field",
              Label(
                Label.Props("Sequence Complete",
                            color = "green".some,
                            icon  = IconCheckmark.some,
                            size  = Size.Big))
            ).when(status === SequenceState.Completed),
            <.div(
              ^.cls := "field",
              Label(Label.Props(obsName, basic = true))
            ).when(isLogged),
            <.div(
              ^.cls := "field",
              targetName
            ).when(isLogged)
          )
        )
      }
      .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
