// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import seqexec.model.Step
import seqexec.web.client.actions.FlipSkipStep
import seqexec.web.client.actions.FlipBreakpointStep
import seqexec.web.client.circuit.SeqexecCircuit
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
  final case class Props(clientStatus:       ClientStatus,
                         step:               Step,
                         rowHeight:          Int,
                         obsId:              Observation.Id,
                         firstRunnableIndex: Int,
                         breakPointEnterCB:  Int => Callback,
                         breakPointLeaveCB:  Int => Callback,
                         heightChangeCB:     Int => Callback)

  implicit val propsReuse: Reusability[Props] =
    Reusability.caseClassExcept[Props]('heightChangeCB,
                                       'breakPointEnterCB,
                                       'breakPointLeaveCB)

  // Request a to flip the breakpoint
  def flipBreakpoint(p: Props): Callback =
    Callback.when(p.clientStatus.canOperate)(
      SeqexecCircuit.dispatchCB(FlipBreakpointStep(p.obsId, p.step)) >> p
        .heightChangeCB(p.step.id))

  // Request a to flip the skip
  def flipSkipped(p: Props): Callback =
    Callback.when(p.clientStatus.canOperate)(
      SeqexecCircuit.dispatchCB(FlipSkipStep(p.obsId, p.step)))

  private val component = ScalaComponent
    .builder[Props]("StepIconCell")
    .stateless
    .render_P { p =>
      val canSetBreakpoint = p.clientStatus.canOperate && p.step
        .canSetBreakpoint(p.step.id, p.firstRunnableIndex)
      val canSetSkipMark = p.clientStatus.canOperate && p.step.canSetSkipmark
      <.div(
        SeqexecStyles.gutterCell,
        ^.height := p.rowHeight.px,
        <.div(
          SeqexecStyles.breakPointHandleOff.when(p.step.breakpoint),
          SeqexecStyles.breakPointHandleOn.unless(p.step.breakpoint),
          ^.onClick --> flipBreakpoint(p),
          Icon.IconRemove
            .copyIcon(fitted       = true,
                      onMouseEnter = p.breakPointEnterCB(p.step.id),
                      onMouseLeave = p.breakPointLeaveCB(p.step.id),
                      extraStyles  = List(SeqexecStyles.breakPointOffIcon))
            .when(p.step.breakpoint),
          Icon.IconCaretDown
            .copyIcon(fitted       = true,
                      onMouseEnter = p.breakPointEnterCB(p.step.id),
                      onMouseLeave = p.breakPointLeaveCB(p.step.id),
                      extraStyles  = List(SeqexecStyles.breakPointOnIcon))
            .unless(p.step.breakpoint)
        ).when(canSetBreakpoint),
        <.div(
          SeqexecStyles.skipHandle,
          ^.top := (p.rowHeight / 2 - SeqexecStyles.skipHandleHeight + 2).px,
          IconPlusSquareOutline
            .copyIcon(link = true, onClick = flipSkipped(p))
            .when(p.step.skip),
          IconMinusCircle
            .copyIcon(link    = true,
                      color   = Some("orange"),
                      onClick = flipSkipped(p))
            .unless(p.step.skip)
        ).when(canSetSkipMark)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
