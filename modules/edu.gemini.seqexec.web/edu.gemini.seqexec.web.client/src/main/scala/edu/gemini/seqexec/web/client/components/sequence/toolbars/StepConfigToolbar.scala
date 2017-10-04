// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.toolbars

import edu.gemini.seqexec.model.Model.{Instrument, SeqexecSite}
import edu.gemini.seqexec.web.client.circuit.SeqexecCircuit
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.actions.UnShowStep
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconChevronLeft
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import scalacss.ScalaCssReact._

/**
  * Toolbar when displaying a step configuration
  */
object StepConfigToolbar {
  final case class Props(site: SeqexecSite, instrument: Instrument, isLogged: Boolean, step: Int) {
    protected[sequence] val sequenceInfoConnects = site.instruments.list.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(i)))).toMap
  }

  def backToSequence(i: Instrument): Callback = Callback {SeqexecCircuit.dispatch(UnShowStep(i))}

  private val component = ScalaComponent.builder[Props]("StepConfigToolbar")
    .stateless
    .render_P( p =>
      <.div(
        <.div(
          ^.cls := "ui row",
          <.div(
            ^.cls := "left column bottom aligned sixteen wide computer ten wide tablet only",
            p.sequenceInfoConnects.get(p.instrument).whenDefined(c => c(SequenceInfo.apply))
          )
        ),
        <.div(
          ^.cls := "row",
          Button(Button.Props(icon = Some(IconChevronLeft), onClick = backToSequence(p.instrument)), "Back"),
          <.h5(
            ^.cls := "ui header",
            SeqexecStyles.inline,
            s" Configuration for step ${p.step + 1}"
          )
        )
      )
    ).build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
