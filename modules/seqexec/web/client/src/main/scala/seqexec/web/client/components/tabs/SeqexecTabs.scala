// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
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
        val tabsL = x().tabs.toList
        val runningInstruments = tabsL.collect {
          case Right(
              AvailableTab(_, SequenceState.Running(_, _), i, _, _, false, _, _)) =>
            i
        }
        val tabs = tabsL
          .sortBy {
            case Left(_)                 => Int.MinValue
            case Right(t) if t.isPreview => (Int.MinValue + 1)
            case Right(t)                => t.instrument.ordinal
          }
          .map {
            case Right(t) =>
              SequenceTab(
                SequenceTab.Props(p.router,
                                  t,
                                  x().canOperate,
                                  x().defaultObserver,
                                  runningInstruments)): VdomNode
            case Left(t) =>
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
