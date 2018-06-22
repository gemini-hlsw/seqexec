// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import gem.Observation
import seqexec.model.Model.{Instrument, SeqexecSite, StepId}
import seqexec.web.client.model.Pages._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.actions.NavigateSilentTo
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.icon.Icon.IconChevronLeft
import web.client.style._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted

/**
  * Toolbar when displaying a step configuration
  */
object StepConfigToolbar {
  final case class Props(router: RouterCtl[SeqexecPages], site: SeqexecSite, instrument: Instrument, id: Observation.Id, step: StepId) {
    protected[sequence] val sequenceInfoConnects = site.instruments.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(i)))).toMap
  }

  def backToSequence(p: Props): Callback =
    SeqexecCircuit.dispatchCB(NavigateSilentTo(SequencePage(p.instrument, p.id, p.step)))

  private val component = ScalaComponent.builder[Props]("StepConfigToolbar")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui grid",
        <.div(
          ^.cls := "ui row",
          SeqexecStyles.shorterRow,
          <.div(
            ^.cls := "left column bottom aligned sixteen wide computer ten wide tablet only",
            p.sequenceInfoConnects.get(p.instrument).whenDefined(c => c(SequenceInfo.apply))
          )
        ),
        <.div(
          ^.cls := "ui row",
          SeqexecStyles.shorterRow,
          SeqexecStyles.lowerRow,
          <.div(
            ^.cls := "ui left floated eight wide column",
            SeqexecStyles.shorterFields,
            <.div(
              p.router.link(SequencePage(p.instrument, p.id, p.step))
                (Button(Button.Props(icon = Some(IconChevronLeft), labeled = true, onClick = backToSequence(p)), "Back"))
            )
          ),
          <.div(
            ^.cls := "ui right floated eight wide column",
            <.div(
              SeqexecStyles.configLabel,
              <.h5(
                ^.cls := "ui header",
                s" Configuration for step ${p.step + 1}"
              )
            )
          )
        )
      )
    ).build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
