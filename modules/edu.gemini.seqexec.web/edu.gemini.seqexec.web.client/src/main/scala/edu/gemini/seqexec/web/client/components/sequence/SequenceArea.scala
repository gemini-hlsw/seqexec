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
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ReactComponentB, ReactComponentU, ReactNode, ScalazReact, TopNode}
import japgolly.scalajs.react.ScalazReact._

import scalacss.ScalaCssReact._

object StepConfigToolbar {
  case class Props(s: SequenceView, step: Int)

  def backToSequence(s: SequenceView): Callback = Callback {SeqexecCircuit.dispatch(UnShowStep(s))}

  private val component = ReactComponentB[Props]("StepConfigToolbar")
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

  def apply(p: Props): ReactComponentU[Props, Unit, Unit, TopNode] = component(p)
}

object SequenceStepsTableContainer {
  case class Props(s: SequenceView, status: ClientStatus, stepConfigDisplayed: Option[Int])
  case class State(nextStepToRun: Int)

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB

  private val component = ReactComponentB[Props]("SequenceStepsTableContainer")
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

  def apply(s: SequenceView, status: ClientStatus, stepConfigDisplayed: Option[Int]): ReactComponentU[Props, State, Unit, TopNode] = component(Props(s, status, stepConfigDisplayed))
}

/**
  * Content of a single tab with a sequence
  */
object SequenceTabContent {

  case class Props(isActive: Boolean, status: ClientStatus, st: SequenceTab)

  private val component = ReactComponentB[Props]("SequenceTabContent")
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

  def apply(p: Props): ReactComponentU[Props, Unit, Unit, TopNode] = component(p)
}

/**
  * Contains the area with tabs and the sequence body
  */
object SequenceTabsBody {
  case class Props(s: ClientStatus, d: SequencesOnDisplay)
  def tabContents(status: ClientStatus, d: SequencesOnDisplay): Stream[SequenceTabContent.Props] = d.instrumentSequences.map(a => SequenceTabContent.Props(isActive = a == d.instrumentSequences.focus, status, a)).toStream

  private val component = ReactComponentB[Props]("SequenceTabsBody")
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
  val sequencesDisplayConnect: ReactConnectProxy[(ClientStatus, SequencesOnDisplay)] = SeqexecCircuit.connect(SeqexecCircuit.statusAndSequences)
  val headerSideBarConnect: ReactConnectProxy[HeaderSideBarReader] = SeqexecCircuit.connect(SeqexecCircuit.headerSideBarReader)
  private val component = ReactComponentB[Unit]("SequenceHeadersAndTable")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "row",
        <.div(
          ^.cls := "four wide column computer tablet only",
          headerSideBarConnect(HeadersSideBar.apply)
        ),
        sequencesDisplayConnect(SequenceTabsBody.apply)
      )
    ) .build

  def apply(): ReactComponentU[Unit, Unit, Unit, TopNode] = component()
}
/**
  * Contains all the tabs for the sequences available in parallel
  * All connects at this level, be careful about adding connects below here
  */
object SequenceTabs {
  val logConnect: ReactConnectProxy[GlobalLog] = SeqexecCircuit.connect(_.globalLog)

  private val component = ReactComponentB[Unit]("SequenceTabs")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui bottom attached segment",
        <.div(
          ^.cls := "ui two column vertically divided grid",
          SequenceHeadersAndTable(),
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

  def apply(): ReactComponentU[Unit, Unit, Unit, TopNode] = component()
}

object SequenceArea {

  private val component = ReactComponentB[Unit]("QueueTableSection")
    .stateless
    .render( _ =>
      <.div(
        ^.cls := "ui raised segments container",
        TextMenuSegment("Running Sequences", "key.sequences.menu"),
        SequenceTabs()
      )
    ).build

  def apply(): ReactComponentU[Unit, Unit, Unit, TopNode] = component()
}
