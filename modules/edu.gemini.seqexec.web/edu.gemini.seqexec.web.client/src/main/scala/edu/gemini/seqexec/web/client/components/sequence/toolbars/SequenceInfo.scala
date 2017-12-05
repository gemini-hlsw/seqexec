// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.toolbars

import edu.gemini.seqexec.web.client.circuit.StatusAndObserverFocus
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconCheckmark
import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.seqexec.model.Model.SequenceState
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import diode.react.ModelProxy
import scalacss.ScalaCssReact._

import scalaz.syntax.std.option._
import scalaz.syntax.equal._

/**
  * Display the name of the sequence and the observer
  */
object SequenceInfo {
  final case class Props(p: ModelProxy[StatusAndObserverFocus])

  private def component = ScalaComponent.builder[Props]("SequenceInfo")
    .stateless
    .render_P { p =>
      val StatusAndObserverFocus(isLogged, name, _, _, observer, status, tName) = p.p()
      val obsName = name.filter(_.nonEmpty).getOrElse("Unknown.")
      val targetName = tName.filter(_.nonEmpty).getOrElse("DAYCAL")
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
            Label(Label.Props(targetName, basic = true))
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

  def apply(p: ModelProxy[StatusAndObserverFocus]): Unmounted[Props, Unit, Unit] = component(Props(p))
}
