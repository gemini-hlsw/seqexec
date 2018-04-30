// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{SequenceState, SeqexecSite}
import edu.gemini.seqexec.web.client.actions.{NavigateTo, SelectIdToDisplay, SelectInstrumentToDisplay}
import edu.gemini.seqexec.web.client.ModelOps.RunningStep
import edu.gemini.seqexec.web.client.model.Pages.{InstrumentPage, SequencePage, SeqexecPages}
import edu.gemini.seqexec.web.client.circuit.{SeqexecCircuit, InstrumentStatusFocus}
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import cats.implicits._
import scalacss.ScalaCssReact._

object InstrumentTab {
  final case class Props(router: RouterCtl[SeqexecPages], site: SeqexecSite, t: ModelProxy[InstrumentStatusFocus])

  private val component = ScalaComponent.builder[Props]("InstrumentMenu")
    .stateless
    .render_P { p =>
      val tab = p.t()
      val active = tab.active
      val status = tab.idState.map(_._2)
      val hasError = status.exists(_.isError)
      val sequenceId = tab.idState.map(_._1)
      val instrument = tab.instrument
      val tabTitle = tab.runningStep match {
        case Some(RunningStep(current, total)) => s"${sequenceId.getOrElse("")} - ${current + 1}/$total"
        case _                                 => sequenceId.getOrElse("")
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
      val linkPage: SeqexecPages = sequenceId.fold(InstrumentPage(instrument): SeqexecPages)(SequencePage(instrument, _, 0))
      val instrumentNoId =
        <.div(SeqexecStyles.instrumentTabLabel, instrument.show)
      val instrumentWithId =
        <.div(
          SeqexecStyles.instrumentTabLabel,
          <.div(SeqexecStyles.activeInstrumentLabel, instrument.show),
          Label(Label.Props(tabTitle, color = color, icon = icon, extraStyles = List(SeqexecStyles.labelPointer)))
        )
      p.router.link(linkPage)(
        IconAttention.copyIcon(color = Some("red")).when(hasError),
        sequenceId.fold(instrumentNoId)(_ => instrumentWithId),
        ^.cls := "item",
        ^.classSet(
          "active" -> active,
          "error"  -> hasError
        ),
        SeqexecStyles.instrumentTab,
        SeqexecStyles.activeInstrumentContent.when(active),
        SeqexecStyles.errorTab.when(hasError),
        dataTab := instrument.show
      )
    }.componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import edu.gemini.web.client.facades.semanticui.SemanticUITab._

        ctx.getDOMNode.toElement.foreach { dom =>
          $(dom).tab(
            JsTabOptions
              .onVisible { (x: String) =>
                val instrument = ctx.props.site.instruments.toList.find(_.show === x)
                val sequenceId = ctx.props.t().idState.map(_._1)
                val updateModelCB = (sequenceId, instrument) match {
                  case (Some(id), Some(i)) =>
                    ctx.props.t.dispatchCB(NavigateTo(SequencePage(i, id, 0))) >> ctx.props.t.dispatchCB(SelectIdToDisplay(id))
                  case (_, Some(i))        =>
                    ctx.props.t.dispatchCB(NavigateTo(InstrumentPage(i))) >> ctx.props.t.dispatchCB(SelectInstrumentToDisplay(i))
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
  final case class Props(router: RouterCtl[SeqexecPages], site: SeqexecSite) {
    protected[sequence] val instrumentConnects = site.instruments.toList.map(i => SeqexecCircuit.connect(SeqexecCircuit.instrumentStatusReader(i)))
  }

  private val component = ScalaComponent.builder[Props]("InstrumentsMenu")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui attached tabular menu",
        p.instrumentConnects.map(c => c(m => InstrumentTab(InstrumentTab.Props(p.router, p.site, m)))).toTagMod
      )
    ).build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
