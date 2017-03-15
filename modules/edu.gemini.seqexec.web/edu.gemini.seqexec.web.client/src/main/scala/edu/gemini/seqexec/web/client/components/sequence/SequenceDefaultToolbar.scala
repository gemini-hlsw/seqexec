package edu.gemini.seqexec.web.client.components.sequence

import edu.gemini.seqexec.model.Model.{SequenceState, SequenceView}
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.input.InputEV
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import japgolly.scalajs.react.extra.{ExternalVar, TimerSupport}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactComponentU, TopNode}
import japgolly.scalajs.react.ScalazReact._

import scalaz.syntax.equal._
import scalaz.syntax.std.boolean._
import scalaz.std.AllInstances._
import scala.concurrent.duration._

object SequenceObserverField {
  case class Props(s: SequenceView, isLogged: Boolean)

  case class State(currentText: Option[String])


  class Backend(val $: BackendScope[Props, State]) extends TimerSupport {
    def updateObserver(s: SequenceView, name: String) =
      $.props >>= {p => Callback.when(p.isLogged)(Callback(SeqexecCircuit.dispatch(UpdateObserver(s, name)))) }

    def updateState(value: String): Callback =
      $.state >>= {s => Callback.when(s.currentText != Some(s))($.modState(_.copy(currentText = Some(value)))) }

    def submitIfChanged: Callback =
      ($.state zip $.props) >>= {
        case (s, p) => Callback.when(s.currentText != p.s.metadata.observer)(updateObserver(p.s, s.currentText.getOrElse("")))
      }

    def render(p: Props, s: State) = {
      val observerEV = ExternalVar(s.currentText.getOrElse(""))(updateState)
      <.div(
        ^.cls := "ui form",
        <.div(
          ^.cls := "required field",
          Label(Label.Props("Observer", "")),
          InputEV(InputEV.Props(p.s.metadata.instrument + ".observer", p.s.metadata.instrument + ".observer", observerEV, placeholder = "Observer...", disabled = !p.isLogged, onBlur = _ => submitIfChanged))
        )
      )
    }
  }
  private val component = ReactComponentB[Props]("SequenceObserverField")
    .initialState(State(None))
    .renderBackend[Backend]
    .configure(TimerSupport.install)
    .componentWillMount(f => f.backend.$.props >>= {p => f.backend.updateState(p.s.metadata.observer.getOrElse(""))})
    // Every 2 seconds check if the field has changed and submit
    .componentDidMount(c => c.backend.setInterval(c.backend.submitIfChanged, 2.second))
    .componentWillReceiveProps { f =>
      // Update the observer field
      println("UPD " + f.nextProps.s.metadata.observer + "->" + f.$.state.currentText)
      println("UPD " + ((f.nextProps.s.metadata.observer.map(_.toString) =/= f.$.state.currentText) && f.nextProps.s.metadata.observer.nonEmpty))
      Callback.when((f.nextProps.s.metadata.observer =/= f.$.state.currentText) && f.nextProps.s.metadata.observer.nonEmpty)(f.$.modState(_.copy(currentText = f.nextProps.s.metadata.observer)))
    }
    .build

  def apply(p: Props): ReactComponentU[Props, State, Backend, TopNode] = component(p)
}

object SequenceDefaultToolbar {
  case class Props(s: SequenceView, status: ClientStatus, nextStepToRun: Int)
  case class State(runRequested: Boolean, pauseRequested: Boolean)
  private  val ST = ReactS.Fix[State]

  def requestRun(s: SequenceView) =
    ST.retM(Callback { SeqexecCircuit.dispatch(RequestRun(s)) }) >> ST.mod(_.copy(runRequested = true, pauseRequested = false)).liftCB

  def requestPause(s: SequenceView) =
    ST.retM(Callback { SeqexecCircuit.dispatch(RequestPause(s)) }) >> ST.mod(_.copy(runRequested = false, pauseRequested = true)).liftCB

  private val component = ReactComponentB[Props]("SequencesDefaultToolbar")
    .initialState(State(runRequested = false, pauseRequested = false))
    .renderPS( ($, p, s) =>
      <.div(
        ^.cls := "row",
        p.status.isLogged && p.s.status === SequenceState.Completed ?=
          <.h3(
            ^.cls := "ui green header",
            "Sequence completed"
          ),
        <.div(
          ^.cls := "ui two column grid",
          <.div(
            ^.cls := "ui row",
            <.div(
              ^.cls := "left bottom aligned six wide column",
              p.status.isLogged && p.s.hasError ?=
                Button(
                  Button.Props(
                    icon = Some(IconPlay),
                    labeled = true,
                    onClick = $.runState(requestRun(p.s)),
                    color = Some("blue"),
                    dataTooltip = Some(s"${p.s.isPartiallyExecuted ? "Continue" | "Run"} the sequence from the step ${p.nextStepToRun + 1}"),
                    disabled = !p.status.isConnected || s.runRequested),
                  s"${p.s.isPartiallyExecuted ? "Continue" | "Run"} from step ${p.nextStepToRun + 1}"
                ),
              p.status.isLogged && p.s.status === SequenceState.Idle ?=
                Button(
                  Button.Props(
                    icon = Some(IconPlay),
                    labeled = true,
                    onClick = $.runState(requestRun(p.s)),
                    color = Some("blue"),
                    dataTooltip = Some(s"${p.s.isPartiallyExecuted ? "Continue" | "Run"} the sequence from the step ${p.nextStepToRun + 1}"),
                    disabled = !p.status.isConnected || s.runRequested),
                  s"${p.s.isPartiallyExecuted ? "Continue" | "Run"} from step ${p.nextStepToRun + 1}"
                ),
              p.status.isLogged && p.s.status === SequenceState.Running ?=
                Button(
                  Button.Props(
                    icon = Some(IconPause),
                    labeled = true,
                    onClick = $.runState(requestPause(p.s)),
                    color = Some("teal"),
                    dataTooltip = Some("Pause the sequence after the current step completes"),
                    disabled = !p.status.isConnected || s.pauseRequested),
                  "Pause"
                ),
              p.status.isLogged && p.s.status === SequenceState.Paused ?=
                Button(
                  Button.Props(
                    icon = Some(IconPlay),
                    labeled = true,
                    onClick = $.runState(requestPause(p.s)),
                    color = Some("teal"),
                    disabled = !p.status.isConnected),
                  "Continue from step 1"
                )
              ),
              <.div(
                ^.cls := "right column",
                ^.classSet(
                  "ten wide" -> p.status.isLogged,
                  "sixteen wide" -> !p.status.isLogged
                ),
                SequenceObserverField(SequenceObserverField.Props(p.s, p.status.isLogged))
              )
            )
          )
      )
    ).componentWillReceiveProps { f =>
      // Update state of run requested depending on the run state
      Callback.when(f.nextProps.s.status === SequenceState.Running && f.$.state.runRequested)(f.$.modState(_.copy(runRequested = false)))
    }.build

  def apply(p: Props) = component(p)
}
