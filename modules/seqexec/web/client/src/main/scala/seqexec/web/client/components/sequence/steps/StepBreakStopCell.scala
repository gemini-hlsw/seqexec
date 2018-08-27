// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import seqexec.model.Step
import seqexec.web.client.actions.{FlipSkipStep, FlipBreakpointStep}
import seqexec.web.client.circuit.{ SeqexecCircuit, StepsTableFocus }
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.semanticui.elements.icon.Icon
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Component to display an icon for the state
  */
object StepBreakStopCell {
  final case class Props(clientStatus: ClientStatus,
                         focus: StepsTableFocus,
                         step: Step,
                         rowHeight: Int,
                         breakPointEnterCB: Int => Callback,
                         breakPointLeaveCB: Int => Callback,
                         heightChangeCB: Int => Callback) {
    val steps: List[Step] = focus.steps
  }

  implicit val propsReuse: Reusability[Props] = Reusability.caseClassExcept[Props]('heightChangeCB, 'breakPointEnterCB, 'breakPointLeaveCB)

  // Request a to flip the breakpoint
  def flipBreakpoint(p: Props): Callback =
    Callback.when(p.clientStatus.canOperate)(SeqexecCircuit.dispatchCB(FlipBreakpointStep(p.focus.id, p.step)) >> p.heightChangeCB(p.step.id))

  // Request a to flip the skip
  def flipSkipped(p: Props): Callback =
    Callback.when(p.clientStatus.canOperate)(SeqexecCircuit.dispatchCB(FlipSkipStep(p.focus.id, p.step)))

  private def firstRunnableIndex(l: List[Step]): Int = l.zipWithIndex.find(!_._1.isFinished).map(_._2).getOrElse(l.length)

  private val component = ScalaComponent
    .builder[Props]("StepIconCell")
    .stateless
    .render_P { p =>
      val canSetBreakpoint = p.clientStatus.canOperate && p.step.canSetBreakpoint(p.step.id, firstRunnableIndex(p.steps))
      val canSetSkipMark = p.clientStatus.canOperate && p.step.canSetSkipmark
      <.div(
        SeqexecStyles.gutterCell,
        ^.height := p.rowHeight.px,
        <.div(
          SeqexecStyles.breakPointHandleOff.when(p.step.breakpoint),
          SeqexecStyles.breakPointHandleOn.unless(p.step.breakpoint),
          ^.onClick --> flipBreakpoint(p),
          Icon.IconRemove
            .copyIcon(fitted = true,
                      onMouseEnter = p.breakPointEnterCB(p.step.id),
                      onMouseLeave = p.breakPointLeaveCB(p.step.id),
                      extraStyles = List(SeqexecStyles.breakPointOffIcon))
            .when(p.step.breakpoint),
          Icon.IconCaretDown
            .copyIcon(fitted = true,
                      onMouseEnter = p.breakPointEnterCB(p.step.id),
                      onMouseLeave = p.breakPointLeaveCB(p.step.id),
                      extraStyles = List(SeqexecStyles.breakPointOnIcon))
            .unless(p.step.breakpoint)
        ).when(canSetBreakpoint),
        <.div(
          SeqexecStyles.skipHandle,
          ^.top := (p.rowHeight / 2 - SeqexecStyles.skipHandleHeight + 2).px,
          IconPlusSquareOutline
            .copyIcon(link = true, onClick = flipSkipped(p))
            .when(p.step.skip),
          IconMinusCircle
            .copyIcon(link = true,
                      color = Some("orange"),
                      onClick = flipSkipped(p))
            .unless(p.step.skip)
        ).when(canSetSkipMark)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
