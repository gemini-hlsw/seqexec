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
import seqexec.web.client.circuit._
import seqexec.web.client.actions.RequestAllDayCal
import seqexec.web.client.actions.RequestClearAllCal
import seqexec.web.client.model.AddDayCalOperation
import seqexec.web.client.model.ClearAllCalOperation
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.icon.Icon.IconRefresh
import seqexec.web.client.semanticui.elements.icon.Icon.IconCloneOutline
import seqexec.web.client.semanticui.elements.icon.Icon.IconTrashOutline
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
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  def allDayCal(id: QueueId): Callback =
    SeqexecCircuit.dispatchCB(RequestAllDayCal(id))

  def clearAllCal(id: QueueId): Callback =
    SeqexecCircuit.dispatchCB(RequestClearAllCal(id))

  private def addAllButton(p: Props) =
    controlButton(
      icon =
        if (p.addDayCalRunning) IconRefresh.copyIcon(loading = true)
        else IconCloneOutline,
      color    = "blue",
      onClick  = allDayCal(p.queueId),
      disabled = !p.canOperate || p.addDayCalRunning,
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
      disabled = !p.canOperate || p.clearCalRunning,
      tooltip  = "Remove all sequences on the calibration queue",
      text     = "Clear"
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
              clearAllButton(p)
            )
          )
        )
    ))
    .configure(Reusability.shouldComponentUpdate)
    .build

}
