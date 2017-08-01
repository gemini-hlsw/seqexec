package edu.gemini.seqexec.web.client.components.sequence

import diode.ModelR
import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.components.TextMenuSegment
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import edu.gemini.seqexec.model.Model.Instrument
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent, ScalazReact}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalazReact._

import scalaz.syntax.show._

object SequenceStepsTableContainer {
  case class Props(p: ModelProxy[StatusAndStepFocus])
  case class State(nextStepToRun: Int)

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB

  // TODO Consider GN/GS
  private val instrumentConnects = Instrument.gsInstruments.list.toList.map(i => (i, SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(i)))).toMap

  private val component = ScalaComponent.builder[Props]("SequenceStepsTableContainer")
    .initialState(State(0))
    .render_P { p =>
        <.div(
          ^.cls := "ui raised secondary segment",
          p.p().stepConfigDisplayed.fold{
            if (p.p().isLogged)
              SequenceDefaultToolbar(p.p().instrument): VdomElement
            else
              SequenceAnonymousToolbar(p.p().instrument): VdomElement
          }(s => StepConfigToolbar(StepConfigToolbar.Props(p.p().instrument, p.p().isLogged, s))),
          <.div(
            ^.cls := "ui raised secondary segment",
            instrumentConnects.get(p.p().instrument).whenDefined(x => x(m => StepsTableContainer(StepsTableContainer.Props(m, _ => Callback.empty))))
          )
        )
    }.build

  def apply(p: ModelProxy[StatusAndStepFocus]): Unmounted[Props, State, Unit] = component(Props(p))
}

/**
* Content of a single tab with a sequence
*/
object SequenceTabContent {

  case class Props(p: ModelProxy[InstrumentStatusFocus]) {
    val connect = SeqexecCircuit.connect(SeqexecCircuit.statusAndStepReader(p().instrument))
  }

  private val component = ScalaComponent.builder[Props]("SequenceTabContent")
    .stateless
    .render_P { p =>
      val InstrumentStatusFocus(instrument, active, id) = p.p()
      <.div(
        ^.cls := "ui bottom attached tab segment",
        ^.classSet(
          "active" -> active
        ),
        dataTab := instrument.shows,
        id.fold(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)): VdomElement) { _ =>
          p.connect(st => SequenceStepsTableContainer(st): VdomElement)
        }
      )
    }
    .build

    def apply(p: ModelProxy[InstrumentStatusFocus]): Unmounted[Props, Unit, Unit] = component(Props(p))
}

/**
 * Contains the area with tabs and the sequence body
 */
object SequenceTabsBody {
  case class Props(s: ClientStatus, d: SequencesOnDisplay)

  // TODO Consider GN/GS
  private val instrumentConnects = Instrument.gsInstruments.list.toList.map(i => SeqexecCircuit.connect(SeqexecCircuit.instrumentStatusReader(i)))

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

/**
  * Component containing the sidebar on the left and the sequence tabs on the right
  */
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
    )
    .build

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

/**
 * Top level container of the sequence area
 */
object SequenceArea {
  type SequencesModel = ModelR[SeqexecAppRootModel, (ClientStatus, SequencesOnDisplay)]
  type HeadersSideBarModel = ModelR[SeqexecAppRootModel, HeaderSideBarFocus]

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
