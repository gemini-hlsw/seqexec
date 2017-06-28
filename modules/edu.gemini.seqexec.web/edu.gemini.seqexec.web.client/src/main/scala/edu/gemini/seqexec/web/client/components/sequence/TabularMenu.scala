package edu.gemini.seqexec.web.client.components.sequence

import edu.gemini.seqexec.model.Model.{SequenceState, SequenceId, SequenceView, Instrument}
import edu.gemini.seqexec.web.client.model.{NavigateTo, SeqexecCircuit, SelectToDisplay, SelectInstrumentToDisplay, SequencesOnDisplay}
import edu.gemini.seqexec.web.client.model.Pages.InstrumentPage
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}

import scalacss.ScalaCssReact._
import scalaz.syntax.equal._
import scalaz.std.string._
import scalaz.std.option._
import scalaz.syntax.std.option._

/**
  * Menu with tabs
  */
object TabularMenu {
  case class TabItem(instrument: Instrument, id: Option[SequenceId], status: Option[SequenceState], isActive: Boolean, dataItem: String, hasError: Boolean)
  case class Props(d: SequencesOnDisplay) {
    val tabs: List[TabItem] = d.instrumentSequences.map(a => TabItem(a.instrument, a.sequence().map(_.id), a.sequence().map(_.status), isActive = a.instrument === d.instrumentSequences.focus.instrument, a.instrument, a.sequence().map(_.hasError).getOrElse(false))).toStream.toList
  }

  private val component = ScalaComponent.builder[Props]("TabularMenu")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui attached tabular menu",
        p.tabs.map { t =>
          val icon = t.status.flatMap {
            case SequenceState.Running   => IconCircleNotched.copyIcon(loading = true).some
            case SequenceState.Completed => IconCheckmark.some
            case _                       => IconSelectedRadio.some
          }
          val color = t.status.flatMap {
            case SequenceState.Running   => "green".some
            case SequenceState.Completed => "green".some
            case _                       => none[String]
          }
          <.a(
            ^.cls := "item",
            ^.classSet(
              "active" -> t.isActive,
              "error"  -> t.hasError
            ),
            SeqexecStyles.activeInstrumentContent.when(t.id.isDefined),
            SeqexecStyles.errorTab.when(t.hasError),
            dataTab := t.dataItem,
            IconAttention.copyIcon(color = Some("red")).when(t.hasError),
            t.id.map(id => <.div(<.div(SeqexecStyles.activeInstrumentLabel, t.instrument), Label(Label.Props(id, color = color, icon = icon)))).getOrElse(t.instrument)
          )
        }.toTagMod
      )
    ).componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$

        $(ctx.getDOMNode).find(".item").tab(
          JsTabOptions
            .onVisible { (x: Instrument) =>
              val id = ctx.props.tabs.find(_.instrument === x).flatMap(_.id)
              val s: Option[SequenceView] = ctx.props.d.instrumentSequences.toStream.toList.find(_.sequence().map(_.id) === id).flatMap(_.sequence())
              val updateModelCB = s.map(seq => Callback(SeqexecCircuit.dispatch(NavigateTo(InstrumentPage(x, seq.id.some)))) >> Callback(SeqexecCircuit.dispatch(SelectToDisplay(seq))))
                .getOrElse(Callback(SeqexecCircuit.dispatch(NavigateTo(InstrumentPage(x, none)))) >> Callback(SeqexecCircuit.dispatch(SelectInstrumentToDisplay(x))))
              // runNow as we are outside react loop
              updateModelCB.runNow()
            }
        )
      }
    ).build

  def apply(d: SequencesOnDisplay): Unmounted[Props, Unit, Unit] = component(Props(d))
}
