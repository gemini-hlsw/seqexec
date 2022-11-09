// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import cats.syntax.all._
import diode.react.ReactConnectProxy
import japgolly.scalajs.react.React
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import mouse.boolean._
import react.common._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.ButtonGroup
import react.semanticui.elements.button.LabelPosition
import react.semanticui.elements.label.Label
import react.semanticui.sizes._
import seqexec.model.Observation
import seqexec.model.RunningStep
import seqexec.model.enum.Instrument
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.circuit.SequenceInfoFocus
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._
import seqexec.web.client.model.Pages._

final case class StepConfigToolbar(
  router:     RouterCtl[SeqexecPages],
  instrument: Instrument,
  id:         Observation.Id,
  step:       Int,
  total:      Int,
  isPreview:  Boolean
) extends ReactProps[StepConfigToolbar](StepConfigToolbar.component) {
  val sequenceConnect: ReactConnectProxy[Option[SequenceInfoFocus]] =
    SeqexecCircuit.connect(SeqexecCircuit.sequenceObserverReader(id))
}

/**
 * Toolbar when displaying a step configuration
 */
object StepConfigToolbar {
  type Props = StepConfigToolbar

  private val component = ScalaComponent
    .builder[Props]("StepConfigToolbar")
    .stateless
    .render_P { p =>
      val sequencePage = if (p.isPreview) {
        PreviewPage(p.instrument, p.id, StepIdDisplayed(p.step))
      } else {
        SequencePage(p.instrument, p.id, StepIdDisplayed(p.step))
      }
      val nextStepPage = if (p.isPreview) {
        PreviewConfigPage(p.instrument, p.id, p.step + 2)
      } else {
        SequenceConfigPage(p.instrument, p.id, p.step + 2)
      }
      val prevStepPage = if (p.isPreview) {
        PreviewConfigPage(p.instrument, p.id, p.step)
      } else {
        SequenceConfigPage(p.instrument, p.id, p.step)
      }

      <.div(
        SeqexecStyles.ConfigTableControls,
        <.div(SeqexecStyles.SequenceControlButtons)(
          // Back to sequence button
          p.router.link(sequencePage)(
            Button(icon = true,
                   labelPosition = LabelPosition.Left,
                   onClick = p.router.setUrlAndDispatchCB(sequencePage)
            )(IconChevronLeft, "Back")
          )
        ),
        p.sequenceConnect(_() match {
          case Some(p) => SequenceInfo(p)
          case _       => React.Fragment()
        }),
        <.div(SeqexecStyles.SequenceInfo)(
          ButtonGroup(clazz = Css("right floated"))(
            // Previous step button
            (p.step > 0).option(
              p.router.link(prevStepPage)(
                Button(icon = true,
                       labelPosition = LabelPosition.Left,
                       onClick = p.router.setUrlAndDispatchCB(prevStepPage)
                )(IconChevronLeft, "Prev")
              )
            ),
            Label(size = Large, clazz = SeqexecStyles.labelAsButton)(
              RunningStep.fromInt(p.step, p.total).getOrElse(RunningStep.Zero).show
            ),
            // Next step button
            (p.step < p.total - 1).option(
              p.router.link(nextStepPage)(
                Button(icon = true,
                       labelPosition = LabelPosition.Right,
                       onClick = p.router.setUrlAndDispatchCB(nextStepPage)
                )(IconChevronRight, "Next")
              )
            )
          )
        )
      )
    }
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
