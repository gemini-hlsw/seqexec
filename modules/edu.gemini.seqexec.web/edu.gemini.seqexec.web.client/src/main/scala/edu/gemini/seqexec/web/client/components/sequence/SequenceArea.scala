package edu.gemini.seqexec.web.client.components.sequence

import diode.react.{ModelProxy, ReactConnectProxy}

import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.web.client.components.{SeqexecStyles, TabularMenu, TextMenuSegment}
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.divider.Divider
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.semanticui.elements.input.Input

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactNode}
import japgolly.scalajs.react.ScalazReact._

import scalacss.ScalaCssReact._
import scalaz.syntax.equal._
import scalaz.syntax.std.boolean._

object StepConfigToolbar {
  case class Props(s: SequenceView, step: Int)

  def backToSequence(s: SequenceView): Callback = Callback {SeqexecCircuit.dispatch(UnShowStep(s))}

  val component = ReactComponentB[Props]("StepConfigToolbar")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "row",
        Button(Button.Props(icon = Some(IconChevronLeft), onClick = backToSequence(p.s)), "Back"),
        <.h5(
          ^.cls := "ui header",
          SeqexecStyles.inline,
          s" Configuration for step ${p.step + 1}"
        )
      )
    ).build

  def apply(p: Props) = component(p)
}

object SequenceDefaultToolbar {
  case class Props(s: SequenceView, status: ClientStatus, nextStepToRun: Int)
  case class State(runRequested: Boolean, pauseRequested: Boolean)
  val ST = ReactS.Fix[State]

  def requestRun(s: SequenceView) =
    ST.retM(Callback { SeqexecCircuit.dispatch(RequestRun(s)) }) >> ST.mod(_.copy(runRequested = true, pauseRequested = false)).liftCB

  def requestPause(s: SequenceView) =
    ST.retM(Callback { SeqexecCircuit.dispatch(RequestPause(s)) }) >> ST.mod(_.copy(runRequested = false, pauseRequested = true)).liftCB

  def updateObserver(s: SequenceView, name: String) =
    Callback(SeqexecCircuit.dispatch(UpdateObserver(s, name)))

  val component = ReactComponentB[Props]("SequencesDefaultToolbar")
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
                <.div(
                  ^.cls := "ui form",
                  <.div(
                    ^.cls := "required field",
                    Label(Label.Props("Observer", "")),
                    Input(Input.Props(p.s.metadata.instrument + ".observer", p.s.metadata.instrument + ".observer", p.s.metadata.observer.getOrElse(""), placeholder = "Observer...", disabled = !p.status.isLogged, onBlur = name => updateObserver(p.s, name)))
                  )
                )
              )
            )
          )
      )
    ).componentWillReceiveProps { f =>
      // Update state of run requested depending on the run state
      val runStateCB =
        Callback.when(f.nextProps.s.status === SequenceState.Running && f.$.state.runRequested)(f.$.modState(_.copy(runRequested = false)))
      runStateCB
    }.build

  def apply(p: Props) = component(p)
}

object SequenceStepsTableContainer {
  case class Props(s: SequenceView, status: ClientStatus, stepConfigDisplayed: Option[Int])
  case class State(nextStepToRun: Int)

  val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int) =
    ST.set(State(step)).liftCB

  val component = ReactComponentB[Props]("SequenceStepsTableContainer")
    .initialState(State(0))
    .renderPS { ($, p, s) =>
      <.div(
        ^.cls := "ui raised secondary segment",
        p.stepConfigDisplayed.fold(SequenceDefaultToolbar(SequenceDefaultToolbar.Props(p.s, p.status, s.nextStepToRun)): ReactNode)(step => StepConfigToolbar(StepConfigToolbar.Props(p.s, step)): ReactNode),
        Divider(),
        StepsTableContainer(StepsTableContainer.Props(p.s, p.status, p.stepConfigDisplayed, s.nextStepToRun, x => $.runState(updateStepToRun(x))))
      )
    }.componentWillMount { f =>
      f.modState(_.copy(nextStepToRun = f.props.s.nextStepToRun.getOrElse(0)))
    }.build

  def apply(s: SequenceView, status: ClientStatus, stepConfigDisplayed: Option[Int]) = component(Props(s, status, stepConfigDisplayed))
}

/**
  * Content of a single tab with a sequence
  */
object SequenceTabContent {

  case class Props(isActive: Boolean, status: ClientStatus, st: SequenceTab)

  val component = ReactComponentB[Props]("SequenceTabContent")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui bottom attached tab segment",
        ^.classSet(
          "active" -> p.isActive
        ),
        dataTab := p.st.instrument,
        p.st.sequence().fold(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)): ReactNode) { s =>
          SequenceStepsTableContainer(s, p.status, p.st.stepConfigDisplayed): ReactNode
        }
      )
    )
    .build

  def apply(p: Props) = component(p)
}

/**
  * Contains the area with tabs and the sequence body
  */
object SequenceTabsBody {
  case class Props(s: ClientStatus, d: SequencesOnDisplay)
  def tabContents(status: ClientStatus, d: SequencesOnDisplay): Stream[SequenceTabContent.Props] = d.instrumentSequences.map(a => SequenceTabContent.Props(isActive = a == d.instrumentSequences.focus, status, a)).toStream

  val component = ReactComponentB[Props]("SequenceTabsBody")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "twelve wide computer twelve wide tablet sixteen wide mobile column",
        TabularMenu(p.d),
        tabContents(p.s, p.d).map(SequenceTabContent.apply)
      )
    )
    .build

  def apply(p: ModelProxy[(ClientStatus, SequencesOnDisplay)]) = component(Props(p()._1, p()._2))
}

object SequenceHeadersAndTable {
  case class Props(proxy: ModelProxy[(ClientStatus, SequencesOnDisplay)])
  val component = ReactComponentB[Props]("SequenceHeadersAndTable")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "row",
        <.div(
          ^.cls := "four wide column computer tablet only",
          HeadersSideBar(None, p.proxy.zoom(_._1))
        ),
        SequenceTabsBody(p.proxy)
      )
    ) .build

  def apply(p: ModelProxy[(ClientStatus, SequencesOnDisplay)]) = component(Props(p))
}
/**
  * Contains all the tabs for the sequences available in parallel
  * All connects at this level, be careful about adding connects below here
  */
object SequenceTabs {
  val logConnect: ReactConnectProxy[GlobalLog] = SeqexecCircuit.connect(_.globalLog)
  val sequencesDisplayConnect: ReactConnectProxy[(ClientStatus, SequencesOnDisplay)] = SeqexecCircuit.connect(SeqexecCircuit.statusAndSequences)

  val component = ReactComponentB[Unit]("SequenceTabs")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui bottom attached segment",
        <.div(
          ^.cls := "ui two column vertically divided grid",
          sequencesDisplayConnect(SequenceHeadersAndTable.apply),
          <.div(
            ^.cls := "row computer only",
            <.div(
              ^.cls := "sixteen wide column",
              logConnect(LogArea.apply)
            )
          )
        )
      )
    )
    .build

  def apply() = component()
}

object SequenceArea {

  val component = ReactComponentB[Unit]("QueueTableSection")
    .stateless
    .render( _ =>
      <.div(
        ^.cls := "ui raised segments container",
        TextMenuSegment("Running Sequences", "key.sequences.menu"),
        SequenceTabs()
      )
    ).build

  def apply() = component()
}
