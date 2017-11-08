// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{SequenceState, SeqexecSite}
import edu.gemini.seqexec.web.client.actions.{NavigateTo, SelectInstrumentToDisplay}
import edu.gemini.seqexec.web.client.model.Pages.InstrumentPage
import edu.gemini.seqexec.web.client.circuit.{SeqexecCircuit, InstrumentStatusFocus}
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}

import scalacss.ScalaCssReact._

import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.std.option._
import scalaz.syntax.show._
import scalaz.syntax.equal._

object InstrumentTab {
  final case class Props(site: SeqexecSite, t: ModelProxy[InstrumentStatusFocus])

  private val component = ScalaComponent.builder[Props]("InstrumentMenu")
    .stateless
    .render_P { p =>
      val tab = p.t()
      val active = tab.active
      val status = tab.idState.map(_._2)
      val hasError = status.exists(SequenceState.isError)
      val sequenceId = tab.idState.map(_._1)
      val instrument = tab.instrument
      val tabTitle = tab.runningStep match {
        case Some((current, total)) => s"${~sequenceId} - ${current + 1}/$total"
        case _                      => ~sequenceId
      }
      val icon = status.flatMap {
        case SequenceState.Running   => IconCircleNotched.copyIcon(loading = true).some
        case SequenceState.Completed => IconCheckmark.some
        case _                       => IconSelectedRadio.some
      }
      val color = status.flatMap {
        case SequenceState.Running   => "warning".some
        case SequenceState.Completed => "green".some
        case _                       => "grey".some
      }
      val instrumentNoId =
        <.div(SeqexecStyles.instrumentTabLabel, instrument.shows)
      val instrumentWithId =
       <.div(SeqexecStyles.instrumentTabLabel, <.div(SeqexecStyles.activeInstrumentLabel, instrument.shows), Label(Label.Props(tabTitle, color = color, icon = icon)))

      <.a(
        ^.cls := "item",
        ^.classSet(
          "active" -> active,
          "error"  -> hasError
        ),
        SeqexecStyles.instrumentTab,
        SeqexecStyles.activeInstrumentContent.when(active),
        SeqexecStyles.errorTab.when(hasError),
        dataTab := instrument.shows,
        IconAttention.copyIcon(color = Some("red")).when(hasError),
        sequenceId.fold(instrumentNoId)(_ => instrumentWithId)
      )
    }.componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import edu.gemini.web.client.facades.semanticui.SemanticUITab._

        $(ctx.getDOMNode).tab(
          JsTabOptions
            .onVisible { (x: String) =>
              val instrument = ctx.props.site.instruments.list.toList.find(_.shows === x)
              val updateModelCB = (ctx.props.t().idState, instrument) match {
                case (_, Some(i))             =>
                  ctx.props.t.dispatchCB(NavigateTo(InstrumentPage(i, none))) >> ctx.props.t.dispatchCB(SelectInstrumentToDisplay(i))
                case _                        =>
                  Callback.empty
              }
              // runNow as we are outside react loop
              updateModelCB.runNow()
            }
        )
      }
    ).build

  def apply(site: SeqexecSite, p: ModelProxy[InstrumentStatusFocus]): Unmounted[Props, Unit, Unit] = component(Props(site, p))
}
/**
  * Menu with tabs
  */
object InstrumentsTabs {
  final case class Props(site: SeqexecSite) {
    protected[sequence] val instrumentConnects = site.instruments.list.toList.map(i => SeqexecCircuit.connect(SeqexecCircuit.instrumentStatusReader(i)))
  }

  private val component = ScalaComponent.builder[Props]("InstrumentsMenu")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui attached tabular menu",
        p.instrumentConnects.map(c => c(InstrumentTab(p.site, _))).toTagMod
      )
    ).build

  def apply(site: SeqexecSite): Unmounted[Props, Unit, Unit] = component(Props(site))
}
