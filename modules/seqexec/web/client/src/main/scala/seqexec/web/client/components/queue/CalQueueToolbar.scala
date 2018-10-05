// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.queue

import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import seqexec.model.QueueId
// import seqexec.model.enum.BatchCommandState
import seqexec.web.client.circuit._
import seqexec.web.client.actions.RequestAllDayCal
import seqexec.web.client.actions.RequestClearAllCal
import seqexec.web.client.actions.RequestRunCal
import seqexec.web.client.actions.RequestStopCal
import seqexec.web.client.model.AddDayCalOperation
import seqexec.web.client.model.RunCalOperation
import seqexec.web.client.model.StopCalOperation
import seqexec.web.client.model.ClearAllCalOperation
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.icon.Icon.IconRefresh
import seqexec.web.client.semanticui.elements.icon.Icon.IconCloneOutline
import seqexec.web.client.semanticui.elements.icon.Icon.IconTrashOutline
import seqexec.web.client.semanticui.elements.icon.Icon.IconPlay
import seqexec.web.client.semanticui.elements.icon.Icon.IconStop
import seqexec.web.client.semanticui.controlButton
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Toolbar for logged in users
  */
object CalQueueToolbar {

  final case class Props(queueId: QueueId, control: CalQueueControlFocus) {
    def cmp: Unmounted[Props, Unit, Unit] = component(this)
    val canOperate: Boolean               = control.canOperate

    val clearCalRunning: Boolean =
      control.ops.clearAllCalRequested === ClearAllCalOperation.ClearAllCalInFlight

    val addDayCalRunning: Boolean =
      control.ops.addDayCalRequested === AddDayCalOperation.AddDayCalInFlight

    val runRunning: Boolean =
      control.ops.runCalRequested === RunCalOperation.RunCalInFlight

    val stopRunning: Boolean =
      control.ops.stopCalRequested === StopCalOperation.StopCalInFlight

    val anyRunning: Boolean =
      clearCalRunning || addDayCalRunning || runRunning || stopRunning

    val queueRunning: Boolean = control.state.running
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def allDayCal(id: QueueId): Callback =
    SeqexecCircuit.dispatchCB(RequestAllDayCal(id))

  def clearAllCal(id: QueueId): Callback =
    SeqexecCircuit.dispatchCB(RequestClearAllCal(id))

  def runCal(id: QueueId): Callback =
    SeqexecCircuit.dispatchCB(RequestRunCal(id))

  def stopCal(id: QueueId): Callback =
    SeqexecCircuit.dispatchCB(RequestStopCal(id))

  private def addAllButton(p: Props) =
    controlButton(
      icon =
        if (p.addDayCalRunning) IconRefresh.copyIcon(loading = true)
        else IconCloneOutline,
      color    = "blue",
      onClick  = allDayCal(p.queueId),
      disabled = !p.canOperate || p.anyRunning || p.queueRunning,
      tooltip  = "Add all sequences on the session queue",
      text     = "Add all"
    )

  private def clearAllButton(p: Props) =
    controlButton(
      icon =
        if (p.clearCalRunning) IconRefresh.copyIcon(loading = true)
        else IconTrashOutline,
      color    = "brown",
      onClick  = clearAllCal(p.queueId),
      disabled = !p.canOperate || p.anyRunning || p.queueRunning,
      tooltip  = "Remove all sequences on the calibration queue",
      text     = "Clear"
    )

  private def runButton(p: Props) =
    controlButton(
      icon =
        if (p.runRunning) IconRefresh.copyIcon(loading = true)
        else IconPlay,
      color    = "blue",
      onClick  = runCal(p.queueId),
      disabled = !p.canOperate || p.anyRunning,
      tooltip  = "Run the calibration queue",
      text     = "Run"
    )

  private def stopButton(p: Props) =
    controlButton(
      icon =
        if (p.runRunning) IconRefresh.copyIcon(loading = true)
        else IconStop,
      color    = "teal",
      onClick  = stopCal(p.queueId),
      disabled = !p.canOperate || p.anyRunning,
      tooltip  = "Stop the calibration queue",
      text     = "Stop"
    )

  private val component = ScalaComponent
    .builder[Props]("CalQueueToolbar")
    .render_P(p =>
      <.div(
        ^.cls := "ui grid",
        <.div(
          ^.cls := "row",
          SeqexecStyles.shorterRow,
          <.div(
            ^.cls := "ui left floated column",
            <.div(
              SeqexecStyles.controlButtons,
              addAllButton(p),
              clearAllButton(p),
              runButton(p).unless(p.queueRunning),
              stopButton(p).when(p.queueRunning)
            )
          )
        )
    ))
    .configure(Reusability.shouldComponentUpdate)
    .build

}
