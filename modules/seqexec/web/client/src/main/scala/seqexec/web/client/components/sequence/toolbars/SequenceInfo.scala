// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import seqexec.web.client.circuit.SequenceInfoFocus
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.elements.icon.Icon.IconCheckmark
import seqexec.web.client.semanticui.Size
import seqexec.model.{ SequenceState, DaytimeCalibrationTargetName }
import web.client.style._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import diode.react.ModelProxy
import cats.implicits._

/**
  * Display the name of the sequence and the observer
  */
object SequenceInfo {
  final case class Props(p: ModelProxy[SequenceInfoFocus])

  private def component = ScalaComponent.builder[Props]("SequenceInfo")
    .stateless
    .render_P { p =>
      val SequenceInfoFocus(isLogged, oName, observer, status, tName) = p.p()
      val obsName = oName.filter(_.nonEmpty).getOrElse("Unknown.")
      val daytimeCalibrationTargetName: TagMod =
        Label(Label.Props(DaytimeCalibrationTargetName, basic = true, extraStyles = List(SeqexecStyles.daytimeCal)))
      val targetName = tName.filter(_.nonEmpty).fold(daytimeCalibrationTargetName)(t => Label(Label.Props(t, basic = true)))
      <.div(
        ^.cls := "ui form",
        <.div(
          ^.cls := "fields",
          SeqexecStyles.fieldsNoBottom,
          <.div(
            ^.cls := "field",
            Label(Label.Props("Sequence Complete", color = "green".some, icon = IconCheckmark.some, size = Size.Big))
          ).when(status.forall(_ === SequenceState.Completed)),
          <.div(
            ^.cls := "field",
            Label(Label.Props(obsName, basic = true))
          ).when(isLogged),
          <.div(
            ^.cls := "field",
            targetName
          ).when(isLogged),
          <.div(
            ^.cls := "field",
            Label(Label.Props("Observer:", basic = true, color = "red".some))
          ).unless(isLogged),
          <.div(
            ^.cls := "field",
            Label(Label.Props(observer.map(_.value).getOrElse("Unknown."), basic = true))
          ).unless(isLogged)
        )
      )
    }.build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
