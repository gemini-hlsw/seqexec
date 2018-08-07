// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import seqexec.model.enum.Instrument
import seqexec.web.client.model.Pages._
import seqexec.web.client.ModelOps._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.actions.NavigateSilentTo
import seqexec.web.client.semanticui.elements.button.ButtonGroup
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.button.Button._
import seqexec.web.client.semanticui.elements.icon.Icon.{IconChevronLeft, IconChevronRight}
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.Size
import web.client.style._
import mouse.boolean._

/**
  * Toolbar when displaying a step configuration
  */
object StepConfigToolbar {
  final case class Props(router: RouterCtl[SeqexecPages], instrument: Instrument, id: Observation.Id, step: Int, total: Int)

  def backToSequence(p: Props): Callback =
    SeqexecCircuit.dispatchCB(NavigateSilentTo(SequencePage(p.instrument, p.id, p.step)))

  def previousStep(p: Props): Callback =
    SeqexecCircuit.dispatchCB(NavigateSilentTo(SequenceConfigPage(p.instrument, p.id, p.step)))

  def nextStep(p: Props): Callback =
    SeqexecCircuit.dispatchCB(NavigateSilentTo(SequenceConfigPage(p.instrument, p.id, p.step + 1)))

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
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
            SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(p.id))(SequenceInfo.apply)
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
                (Button(Button.Props(icon = Some(IconChevronLeft), labeled = LeftLabeled, onClick = backToSequence(p)), "Back"))
            )
          ),
          <.div(
            ^.cls := "ui right floated eight wide column",
            SeqexecStyles.shorterFields,
            ButtonGroup(
              ButtonGroup.Props(List(GStyle.fromString("right floated"))),
              (p.step > 0).fold(
                (p.router.link(SequenceConfigPage(p.instrument, p.id, p.step))
                  (Button(Button.Props(icon = Some(IconChevronLeft), labeled = LeftLabeled, onClick = previousStep(p)), "Prev"))): VdomElement,
                ReactFragment()),
              Label(Label.Props(RunningStep(p.step, p.total).show, size = Size.Large, extraStyles = List(SeqexecStyles.labelAsButton))),
              (p.step < p.total - 1).fold(
                (p.router.link(SequenceConfigPage(p.instrument, p.id, p.step + 2))
                  (Button(Button.Props(icon = Some(IconChevronRight), labeled = RightLabeled, onClick = nextStep(p)), "Next"))): VdomElement,
                ReactFragment())
            )
          )
        )
      )
    ).build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
