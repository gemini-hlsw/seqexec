// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence

import cats.implicits._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import seqexec.model.SequenceState
import seqexec.web.client.actions.{NavigateTo, SelectIdToDisplay, SelectInstrumentToDisplay}
import seqexec.web.client.ModelOps._
import seqexec.web.client.model.Pages.{InstrumentPage, SequencePage, SeqexecPages}
import seqexec.web.client.model.AvailableTab
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.components.SeqexecStyles
import web.client.style._

object InstrumentTab {
  final case class Props(router: RouterCtl[SeqexecPages], tab: AvailableTab)

  private val component = ScalaComponent.builder[Props]("SequenceTab")
    .stateless
    .render_P { p =>
      val active = p.tab.active
      val status = p.tab.status
      val hasError = status.exists(_.isError)
      val sequenceId = p.tab.id
      val instrument = p.tab.instrument

      val tabTitle = p.tab.runningStep match {
        case Some(RunningStep(current, total)) => s"${sequenceId.map(_.format).getOrElse("")} - ${current + 1}/$total"
        case _                                 => sequenceId.map(_.format).getOrElse("")
      }
      val icon = status.flatMap {
        case SequenceState.Running(_, _) => IconCircleNotched.copyIcon(loading = true).some
        case SequenceState.Completed     => IconCheckmark.some
        case _                           => IconSelectedRadio.some
      }
      val color = status.flatMap {
        case SequenceState.Running(_, _) => "orange".some
        case SequenceState.Completed     => "green".some
        case _                           => "grey".some
      }
      val linkPage: Option[SeqexecPages] = (sequenceId, instrument).mapN((id, inst) => SequencePage(inst, id, 0))
      val instrumentNoId =
        <.div(SeqexecStyles.instrumentTabLabel, instrument.show)
      val instrumentWithId =
        <.div(
          SeqexecStyles.instrumentTabLabel,
          <.div(SeqexecStyles.activeInstrumentLabel, instrument.show),
          Label(Label.Props(tabTitle, color = color, icon = icon, extraStyles = List(SeqexecStyles.labelPointer)))
        )
      val tab = linkPage.map(l => p.router.link(l)(
        IconAttention.copyIcon(color = Some("red")).when(hasError),
        sequenceId.fold(instrumentNoId)(_ => instrumentWithId),
        ^.cls := "item",
        ^.classSet(
          "active" -> active,
          "error"  -> hasError
        ),
        SeqexecStyles.instrumentTab,
        SeqexecStyles.inactiveInstrumentContent.unless(active),
        SeqexecStyles.activeInstrumentContent.when(active),
        SeqexecStyles.errorTab.when(hasError),
        dataTab := instrument.show
      )).getOrElse(<.div("Preview"))
      tab
    }.componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import web.client.facades.semanticui.SemanticUITab._

        ctx.getDOMNode.toElement.foreach { dom =>
          $(dom).tab(
            JsTabOptions
              .onVisible { (x: String) =>
                println("setup")
                val sequenceId = ctx.props.tab.id
                val instrument = ctx.props.tab.instrument
                val updateModelCB = (sequenceId, instrument) match {
                  case (Some(id), Some(i)) =>
                    SeqexecCircuit.dispatchCB(NavigateTo(SequencePage(i, id, 0))) >> SeqexecCircuit.dispatchCB(SelectIdToDisplay(id))
                  case (_, Some(i))        =>
                    SeqexecCircuit.dispatchCB(NavigateTo(InstrumentPage(i))) >> SeqexecCircuit.dispatchCB(SelectInstrumentToDisplay(i))
                  case _                   =>
                    Callback.empty
                }
                // runNow as we are outside react loop
                updateModelCB.runNow()
              }
          )
        }
      }
    ).build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
/**
  * Menu with tabs
  */
object InstrumentsTabs {
  final case class Props(router: RouterCtl[SeqexecPages])

  private val component = ScalaComponent.builder[Props]("InstrumentsMenu")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui attached tabular menu",
        SeqexecCircuit.connect(SeqexecCircuit.availableTabs)(x => ReactFragment(x().toList.map(t => InstrumentTab(InstrumentTab.Props(p.router, t)): VdomNode): _*))
      )
    ).build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
