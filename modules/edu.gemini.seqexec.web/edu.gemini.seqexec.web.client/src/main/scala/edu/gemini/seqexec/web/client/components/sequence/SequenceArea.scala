package edu.gemini.seqexec.web.client.components.sequence

import diode.react.{ModelProxy, ReactConnectProxy}
import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.components.{TabularMenu, TextMenuSegment}
import edu.gemini.seqexec.web.client.components.SeqexecUI.RouterProps
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CallbackTo, ScalaComponent, ScalazReact}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalazReact._

import scalaz.syntax.equal._
import scalaz.std.string._

object SequenceStepsTableContainer {
  case class Props(s: SequenceView, status: ClientStatus, stepConfigDisplayed: Option[Int])
  case class State(nextStepToRun: Int)

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB

  private val component = ScalaComponent.builder[Props]("SequenceStepsTableContainer")
    .initialState(State(0))
    .renderPS { ($, p, s) =>
      <.div(
        ^.cls := "ui raised secondary segment",
        p.stepConfigDisplayed.fold{
          (if (p.status.isLogged) SequenceDefaultToolbar(SequenceDefaultToolbar.Props(p.s, p.status, s.nextStepToRun)) else SequenceAnonymousToolbar(SequenceAnonymousToolbar.Props(p.s))): VdomNode}
          (step => StepConfigToolbar(StepConfigToolbar.Props(p.s, p.status.isLogged,step)): VdomNode),
        StepsTableContainer(StepsTableContainer.Props(p.s, p.status, p.stepConfigDisplayed, s.nextStepToRun, x => $.runState(updateStepToRun(x))))
      )
    }.componentWillMount { f =>
      f.modState(_.copy(nextStepToRun = f.props.s.nextStepToRun.getOrElse(0)))
    }.build

  def apply(s: SequenceView, status: ClientStatus, stepConfigDisplayed: Option[Int]): Unmounted[Props, State, Unit] = component(Props(s, status, stepConfigDisplayed))
}

/**
  * Content of a single tab with a sequence
  */
object SequenceTabContent {

  case class Props(isActive: Boolean, status: ClientStatus, st: SequenceTab)

  private val component = ScalaComponent.builder[Props]("SequenceTabContent")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui bottom attached tab segment",
        ^.classSet(
          "active" -> p.isActive
        ),
        dataTab := p.st.instrument,
        p.st.sequence().fold(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)): VdomNode) { s =>
          SequenceStepsTableContainer(s, p.status, p.st.stepConfigDisplayed): VdomNode
        }
      )
    )
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}

/**
  * Contains the area with tabs and the sequence body
  */
object SequenceTabsBody {
  case class Props(p: RouterProps, s: ClientStatus, d: SequencesOnDisplay)
  def tabContents(p: Props): Stream[SequenceTabContent.Props] =
    p.d.instrumentSequences.map { a =>
      SequenceTabContent.Props(isActive = a.instrument === p.p.page.i, p.s, a)}.toStream

  private val component = ScalaComponent.builder[Props]("SequenceTabsBody")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "twelve wide computer twelve wide tablet sixteen wide mobile column",
        TabularMenu(p.p, p.d),
        tabContents(p).map(SequenceTabContent.apply).toTagMod
      )
    )
    .build

  def apply(page: RouterProps, p: ModelProxy[(ClientStatus, SequencesOnDisplay)]): Unmounted[Props, Unit, Unit] = component(Props(page, p()._1, p()._2))
}

object SequenceHeadersAndTable {
  val sequencesDisplayConnect: ReactConnectProxy[(ClientStatus, SequencesOnDisplay)] = SeqexecCircuit.connect(SeqexecCircuit.statusAndSequences)
  val headerSideBarConnect: ReactConnectProxy[HeaderSideBarReader] = SeqexecCircuit.connect(SeqexecCircuit.headerSideBarReader)
  private val component = ScalaComponent.builder[RouterProps]("SequenceHeadersAndTable")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "row",
        <.div(
          ^.cls := "four wide column computer tablet only",
          headerSideBarConnect(HeadersSideBar.apply)
        ),
        sequencesDisplayConnect(proxy => SequenceTabsBody(p, proxy))
      )
    ) .build

  def apply(p: RouterProps): Unmounted[RouterProps, Unit, Unit] = component(p)
}

/**
  * Contains all the tabs for the sequences available in parallel
  * All connects at this level, be careful about adding connects below here
  */
object SequenceTabs {
  val logConnect: ReactConnectProxy[GlobalLog] = SeqexecCircuit.connect(_.globalLog)

  private val component = ScalaComponent.builder[RouterProps]("SequenceTabs")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui bottom attached segment",
        <.div(
          ^.cls := "ui two column vertically divided grid",
          SequenceHeadersAndTable(p),
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

  def apply(p: RouterProps): Unmounted[RouterProps, Unit, Unit] = component(p)
}

object SequenceArea {

  private val component = ScalaComponent.builder[RouterProps]("QueueTableSection")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui raised segments container",
        TextMenuSegment("Running Sequences", "key.sequences.menu"),
        SequenceTabs(p)
      )
    ).build

  def apply(props: RouterProps): Unmounted[RouterProps, Unit, Unit] = component(props)
}
