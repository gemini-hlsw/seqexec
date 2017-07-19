package edu.gemini.seqexec.web.client.components.sequence

import diode.ModelR
import diode.react.ModelProxy
// import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.components.TextMenuSegment
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.model.Model.Instrument
import edu.gemini.seqexec.web.client.semanticui._
// import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
// import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import edu.gemini.seqexec.model.Model.Instrument
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CallbackTo, ScalaComponent, ScalazReact}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalazReact._

// import scalaz.syntax.apply.{^ => _, _}
import scalaz.syntax.show._
// import scalaz.std.option._

object SequenceStepsTableContainer {
  case class Props(p: Instrument)
  case class State(nextStepToRun: Int)

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB

  // TODO Consider GN/GS
  val instrumentConnects = Instrument.gsInstruments.list.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.instrumentStepsTable(i)))).toMap

  private val component = ScalaComponent.builder[Props]("SequenceStepsTableContainer")
    .initialState(State(0))
    .renderPS { ($, p, s) =>
      // val status = p.p().status
      /*val sequence = p.p().tab.flatMap(_.sequence)
      sequence.map { case (s, stepConfigDisplayed) =>
        <.div(
          ^.cls := "ui raised secondary segment",
          stepConfigDisplayed.fold{
            (if (status.isLogged)
            SequenceDefaultToolbar(SequenceDefaultToolbar.Props(s, status, s.nextStepToRun))
            else
            SequenceAnonymousToolbar(SequenceAnonymousToolbar.Props(s))): VdomElement
          }
          (step => StepConfigToolbar(StepConfigToolbar.Props(s, status.isLogged, step))): VdomElement,
          StepsTableContainer(StepsTableContainer.Props(s.id, s.metadata.instrument, s.steps, status, stepConfigDisplayed, s.nextStepToRun, x => $.runState(updateStepToRun(x))))
        ): VdomElement
      }.getOrElse(<.div(): VdomElement)*/
        <.div(
          ^.cls := "ui raised secondary segment"//,
          // instrumentConnects.get(p.p).map(x => x.map(StepsTableContainer(StepsTableContainer.Props(x, _ => Callback.empty)))).getOrElse(<.div(): VdomElement)
        )
      // <.div()
    }.componentWillMount { f =>
      /*val nextStep = for {
        t      <- f.props.p().tab
        (s, _) <- t.sequence
        n      <- s.nextStepToRun
      } yield n
      f.modState(_.copy(nextStepToRun = nextStep.getOrElse(0)))*/
      japgolly.scalajs.react.Callback.empty
    }.build

  def apply(p: Instrument): Unmounted[Props, State, Unit] = component(Props(p))
}

/**
* Content of a single tab with a sequence
*/
object SequenceTabContent {

  case class Props(p: ModelProxy[InstrumentActive])

  private val component = ScalaComponent.builder[Props]("SequenceTabContent")
    .stateless
    .render_P { p =>
      val InstrumentActive(_, instrument, active) = p.p()
      <.div(
        ^.cls := "ui bottom attached tab segment",
        ^.classSet(
          "active" -> active
        ),
        dataTab := instrument.shows//,
        // sequence.fold(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)): VdomNode) { s =>
        //   SequenceStepsTableContainer(p.p): VdomNode
        // }
      )
    }
    .build

    def apply(p: ModelProxy[InstrumentActive]) = component(Props(p))
}

/**
* Contains the area with tabs and the sequence body
*/
object SequenceTabsBody {
  case class Props(s: ClientStatus, d: SequencesOnDisplay)

  // TODO Consider GN/GS
  val instrumentConnects = Instrument.gsInstruments.list.toList.map(i => SeqexecCircuit.connect(SeqexecCircuit.instrumentActive(i)))

  private val component = ScalaComponent.builder[Unit]("SequenceTabsBody")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "twelve wide computer twelve wide tablet sixteen wide mobile column",
        InstrumentsTabs(),
        instrumentConnects.map(c => c(SequenceTabContent.apply)).toTagMod
      )
    ).build

  def apply(): Unmounted[Unit, Unit, Unit] =
    component()
}

object SequenceHeadersAndTable {
  private val headerSideBarConnect = SeqexecCircuit.connect(SeqexecCircuit.headerSideBarReader)

  private val component = ScalaComponent.builder[Unit]("SequenceHeadersAndTable")
  .stateless
  .render_P(p =>
    <.div(
      ^.cls := "row",
      <.div(
        ^.cls := "four wide column computer tablet only",
        headerSideBarConnect(HeadersSideBar.apply)
      ),
      SequenceTabsBody()
    )
  ) .build

  def apply(): Unmounted[Unit, Unit, Unit] = component()
}

/**
* Contains all the tabs for the sequences available in parallel
* All connects at this level, be careful about adding connects below here
*/
object SequenceTabs {
  private val component = ScalaComponent.builder[Unit]("SequenceTabs")
  .stateless
  .render_P( p =>
    <.div(
      ^.cls := "ui bottom attached segment",
      <.div(
        ^.cls := "ui two column vertically divided grid",
        SequenceHeadersAndTable()
      )
    )
  )
  .build

  def apply(): Unmounted[Unit, Unit, Unit] = component()
}

object SequenceArea {
  type SequencesModel = ModelR[SeqexecAppRootModel, (ClientStatus, SequencesOnDisplay)]
  type HeadersSideBarModel = ModelR[SeqexecAppRootModel, HeaderSideBarReader]

  private val component = ScalaComponent.builder[Unit]("QueueTableSection")
  .stateless
  .render_P( p =>
    <.div(
      ^.cls := "ui raised segments container",
      TextMenuSegment("Running Sequences", "key.sequences.menu"),
      SequenceTabs()
    )
  ).build

  def apply(): Unmounted[Unit, Unit, Unit] = component()
}
