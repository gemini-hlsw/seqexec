// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.colors._
import seqexec.model.Observation
import seqexec.model.Step
import seqexec.web.client.actions.FlipBreakpointStep
import seqexec.web.client.actions.FlipSkipStep
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.reusability._

/**
 * Component to display an icon for the state
 */
final case class StepBreakStopCell(
  clientStatus:       ClientStatus,
  step:               Step,
  rowHeight:          Int,
  obsId:              Observation.Id,
  firstRunnableIndex: Int,
  breakPointEnterCB:  Int => Callback,
  breakPointLeaveCB:  Int => Callback,
  heightChangeCB:     Int => Callback
) extends ReactProps[StepBreakStopCell](StepBreakStopCell.component)

object StepBreakStopCell {
  type Props = StepBreakStopCell

  implicit val propsReuse: Reusability[Props] =
    Reusability.caseClassExcept[Props]("heightChangeCB", "breakPointEnterCB", "breakPointLeaveCB")

  // Request a to flip the breakpoint
  def flipBreakpoint(p: Props)(e: ReactEvent): Callback =
    e.preventDefaultCB *>
      e.stopPropagationCB *>
      Callback.when(p.clientStatus.canOperate)(
        SeqexecCircuit.dispatchCB(FlipBreakpointStep(p.obsId, p.step)) *>
          p.heightChangeCB(p.step.id)
      )

  // Request a to flip the skip
  def flipSkipped(p: Props)(e: ReactEvent): Callback =
    e.preventDefaultCB *>
      e.stopPropagationCB *>
      Callback.when(p.clientStatus.canOperate)(
        SeqexecCircuit.dispatchCB(FlipSkipStep(p.obsId, p.step))
      )

  protected val component = ScalaComponent
    .builder[Props]("StepBreakStopCell")
    .stateless
    .render_P { p =>
      val canSetBreakpoint = p.clientStatus.canOperate && p.step
        .canSetBreakpoint(p.step.id, p.firstRunnableIndex)
      val canSetSkipMark   = p.clientStatus.canOperate && p.step.canSetSkipmark
      <.div(
        SeqexecStyles.gutterCell,
        ^.height := p.rowHeight.px,
        <.div(
          SeqexecStyles.breakPointHandleOff.when(p.step.breakpoint),
          SeqexecStyles.breakPointHandleOn.unless(p.step.breakpoint),
          ^.onClick ==> flipBreakpoint(p),
          IconRemove
            .copy(fitted = true, clazz = SeqexecStyles.breakPointOffIcon)(
              ^.onMouseEnter --> p.breakPointEnterCB(p.step.id),
              ^.onMouseLeave --> p.breakPointLeaveCB(p.step.id)
            )
            .when(p.step.breakpoint),
          IconCaretDown
            .copy(fitted = true, clazz = SeqexecStyles.breakPointOnIcon)(
              ^.onMouseEnter --> p.breakPointEnterCB(p.step.id),
              ^.onMouseLeave --> p.breakPointLeaveCB(p.step.id)
            )
            .unless(p.step.breakpoint)
        ).when(canSetBreakpoint),
        <.div(
          SeqexecStyles.skipHandle,
          ^.top := (p.rowHeight / 2 - SeqexecStyles.skipHandleHeight + 2).px,
          IconPlusSquareOutline
            .copy(link = true)(^.onClick ==> flipSkipped(p) _)
            .when(p.step.skip),
          IconMinusCircle
            .copy(link = true, color = Orange)(^.onClick ==> flipSkipped(p) _)
            .unless(p.step.skip)
        ).when(canSetSkipMark)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
