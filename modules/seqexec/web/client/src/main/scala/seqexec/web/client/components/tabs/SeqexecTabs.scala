// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.tabs

import cats.implicits._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react._
import seqexec.model.SequenceState
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.AvailableTab
import seqexec.web.client.circuit.SeqexecCircuit

/**
  * Menu with tabs
  */
object SeqexecTabs {
  final case class Props(router: RouterCtl[SeqexecPages])

  implicit val propsReuse: Reusability[Props] = Reusability.always
  private val tabConnect                      = SeqexecCircuit.connect(SeqexecCircuit.tabsReader)

  private val component = ScalaComponent
    .builder[Props]("InstrumentsMenu")
    .stateless
    .render_P(p =>
      tabConnect { x =>
        val runningInstruments = x().tabs.toList.collect {
          case Right(
              AvailableTab(_, Some(SequenceState.Running(_, _)), Some(i), _, _, false, _, _)) =>
            i
        }
        val tabs = x().tabs.toList
          .sortBy {
            case Left(_)                 => Int.MinValue.some
            case Right(t) if t.isPreview => (Int.MinValue + 1).some
            case Right(t)                => t.instrument.map(_.ordinal)
          }
          .map {
            case Right(t) =>
              SequenceTab(SequenceTab.Props(p.router, t, x().canOperate, x().defaultObserver, runningInstruments)): VdomNode
            case Left(t)  =>
              CalibrationQueueTab(CalibrationQueueTab.Props(p.router, t)): VdomNode
          }
        if (tabs.nonEmpty) {
          <.div(
            ^.cls := "ui attached tabular menu",
            React.Fragment(tabs: _*)
          )
        } else {
          <.div()
        }
    })
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
