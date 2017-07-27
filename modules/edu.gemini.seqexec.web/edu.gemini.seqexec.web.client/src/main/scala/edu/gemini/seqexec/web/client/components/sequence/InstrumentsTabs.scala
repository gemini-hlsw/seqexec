package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{SequenceState, Instrument}
import edu.gemini.seqexec.web.client.model.{InstrumentStatus, NavigateTo, SelectIdToDisplay, SelectInstrumentToDisplay}
import edu.gemini.seqexec.web.client.model.Pages.InstrumentPage
import edu.gemini.seqexec.web.client.model.SeqexecCircuit
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.web.client.facades.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}

import scalacss.ScalaCssReact._

import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.show._

object InstrumentTab {
  case class Props(t: ModelProxy[Option[InstrumentStatus]])

  private val component = ScalaComponent.builder[Props]("InstrumentMenu")
    .stateless
    .render_P { q =>
      q.t().fold(<.div(): VdomElement) { case tab =>
        val active = tab.active
        val status = tab.idState.map(_._2)
        val hasError = status.map(SequenceState.isError).getOrElse(false)
        val sequenceId = tab.idState.map(_._1)
        val instrument = tab.instrument
        val icon = status.flatMap {
          case SequenceState.Running   => IconCircleNotched.copyIcon(loading = true).some
          case SequenceState.Completed => IconCheckmark.some
          case _                       => IconSelectedRadio.some
        }
        val color = status.flatMap {
          case SequenceState.Running   => "green".some
          case SequenceState.Completed => "green".some
          case _                       => none[String]
        }
        <.a(
          ^.cls := "item",
          ^.classSet(
            "active" -> active,
            "error"  -> hasError
          ),
          SeqexecStyles.activeInstrumentContent.when(sequenceId.isDefined),
          SeqexecStyles.errorTab.when(hasError),
          dataTab := instrument.shows,
          IconAttention.copyIcon(color = Some("red")).when(hasError),
          sequenceId.map(id => <.div(<.div(SeqexecStyles.activeInstrumentLabel, instrument.shows), Label(Label.Props(id, color = color, icon = icon)))).getOrElse(instrument.shows)
        )
      }
    }.componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$

        $(ctx.getDOMNode).tab(
          JsTabOptions
            .onVisible { (x: Instrument) =>
              ctx.props.t().map(_.idState).foreach { sequence =>
                val updateModelCB = sequence.map(seq => ctx.props.t.dispatchCB(NavigateTo(InstrumentPage(x, seq._1.some))) >> ctx.props.t.dispatchCB(SelectIdToDisplay(seq._1)))
                  .getOrElse(ctx.props.t.dispatchCB(NavigateTo(InstrumentPage(x, none))) >> ctx.props.t.dispatchCB(SelectInstrumentToDisplay(x)))
                // runNow as we are outside react loop
                updateModelCB.runNow()
              }
            }
        )
      }
    ).build

  def apply(p: ModelProxy[Option[InstrumentStatus]]) = component(Props(p))
}
/**
  * Menu with tabs
  */
object InstrumentsTabs {
  // TODO Consider GN/GS
  val instrumentConnects = Instrument.gsInstruments.list.toList.map(i => SeqexecCircuit.connect(SeqexecCircuit.instrumentStatusTab(i)))

  private val component = ScalaComponent.builder[Unit]("InstrumentsMenu")
    .stateless
    .render(_ =>
      <.div(
        ^.cls := "ui attached tabular menu",
        instrumentConnects.map(c => c(InstrumentTab.apply)).toTagMod
      )
    ).build

  def apply(): Unmounted[Unit, Unit, Unit] = component()
}
