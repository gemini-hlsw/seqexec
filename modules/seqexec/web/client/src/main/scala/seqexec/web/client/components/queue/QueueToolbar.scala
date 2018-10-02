// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.queue

import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.CatsReact
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import monocle.macros.Lenses
import seqexec.model.QueueId
import seqexec.web.client.circuit._
import seqexec.web.client.actions.RequestAllDayCal
import seqexec.web.client.model.AddDayCalOperation
import seqexec.web.client.model.QueueOperations
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.icon.Icon.IconRefresh
import seqexec.web.client.semanticui.elements.icon.Icon.IconCloneOutline
import seqexec.web.client.semanticui.controlButton
import seqexec.web.client.reusability._
import web.client.style._

/**
  * Toolbar for logged in users
  */
object QueueToolbar {
  final case class Props(queueId: QueueId, control: QueueControlFocus) {
    def cmp: Unmounted[Props, State, Unit] = component(this)
  }

  @Lenses
  final case class State(ops: QueueOperations) {
    def requestAddDayCal: State =
      (State.ops ^|-> QueueOperations.addDayCalRequested)
        .set(AddDayCalOperation.AddDayCalInFlight)(this)

    val addDayCalRunning: Boolean =
      ops.addDayCalRequested === AddDayCalOperation.AddDayCalInFlight
  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object State {
    val Zero: State = State(ops = QueueOperations.Default)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val stateReuse: Reusability[State] = Reusability.derive[State]

  private val ST = ReactS.Fix[State]

  def allDayCal(id: QueueId): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.retM(SeqexecCircuit.dispatchCB(RequestAllDayCal(id))) >>
      ST.mod(_.requestAddDayCal).liftCB

  private val component = ScalaComponent
    .builder[Props]("QueueToolbar")
    .initialState(State.Zero)
    .renderPS{(b, p, s) =>
      val canOperate = p.control.canOperate
      <.div(
        ^.cls := "ui grid",
        <.div(
          ^.cls := "row",
          SeqexecStyles.shorterRow,
          <.div(
            ^.cls := "ui left floated column",
            controlButton(
              icon =
                if (s.addDayCalRunning) IconRefresh.copyIcon(loading = true)
                else IconCloneOutline,
              color    = "blue",
              onClick  = b.runState(allDayCal(p.queueId)),
              disabled = !canOperate || s.addDayCalRunning,
              tooltip  = "Add all daytime calibrations on the session queue",
              text     = "Add all day cal"
            )
          )
        )
    )}
    .componentWillReceiveProps { f =>
      // Update state of operations
      Callback.when(f.nextProps.control.ops =!= f.state.ops)(
        f.modState(_.copy(ops = f.nextProps.control.ops)))
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

}
