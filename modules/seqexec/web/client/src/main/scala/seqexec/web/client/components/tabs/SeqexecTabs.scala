// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.tabs

import cats.implicits._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import seqexec.model.SequenceState
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.AvailableTab
import seqexec.web.client.circuit.SeqexecCircuit
import react.common._
import react.semanticui.collections.menu.Menu
import react.semanticui.collections.menu.MenuAttached
import react.semanticui.collections.menu.MenuTabular

/**
  * Menu with tabs
  */
final case class SeqexecTabs(
  router: RouterCtl[SeqexecPages]
) extends ReactProps[SeqexecTabs](SeqexecTabs.component)

object SeqexecTabs {
  type Props = SeqexecTabs

  implicit val propsReuse: Reusability[Props] = Reusability.always
  private val tabConnect                      = SeqexecCircuit.connect(SeqexecCircuit.tabsReader)

  val component = ScalaComponent
    .builder[Props]("InstrumentsMenu")
    .stateless
    .render_P(p =>
      tabConnect { x =>
        val tabsL = x().tabs.toList
        val runningInstruments = tabsL.collect {
          case Right(AvailableTab(_, SequenceState.Running(_, _), i, _, _, false, _, _, _)) =>
            i
        }
        val tabs: List[VdomNode] =
          tabsL
            .sortBy {
              case Left(_)                 => Int.MinValue
              case Right(t) if t.isPreview => (Int.MinValue + 1)
              case Right(t)                => t.instrument.ordinal
            }
            .map {
              case Right(t) =>
                SequenceTab(p.router, t, x().canOperate, x().defaultObserver, runningInstruments)
              case Left(t) =>
                CalibrationQueueTab(p.router, t)
            }
        React.Fragment(
          Menu(tabular = MenuTabular.Tabular, attached = MenuAttached.Attached)(
            tabs: _*
          )
        )
      }
    )
    .configure(Reusability.shouldComponentUpdate)
    .build
}
