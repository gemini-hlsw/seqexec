// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.queue

import cats.syntax.all._
import japgolly.scalajs.react.React
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.As
import react.semanticui.collections.message.Message
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentAttached
import react.semanticui.modules.tab.TabPane
import seqexec.model.CalibrationQueueId
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._
import seqexec.web.client.model.SectionVisibilityState
import seqexec.web.client.model.TabSelected
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui.dataTab

final case class CalQueueTabContent(
  canOperate:   Boolean,
  active:       TabSelected,
  logDisplayed: SectionVisibilityState
) extends ReactProps[CalQueueTabContent](CalQueueTabContent.component) {
  protected[queue] val dayCalConnectOps =
    SeqexecCircuit.connect(SeqexecCircuit.calQueueControlReader(CalibrationQueueId))
  protected[queue] val dayCalConnect    =
    SeqexecCircuit.connect(SeqexecCircuit.calQueueReader(CalibrationQueueId))

  val isActive: Boolean =
    active === TabSelected.Selected
}

/**
 * Content of the queue tab
 */
object CalQueueTabContent {
  type Props = CalQueueTabContent

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val defaultContent =
    Message(
      icon = true,
      warning = true
    )(
      IconInbox,
      "Work in progress"
    ).render

  private val component = ScalaComponent
    .builder[Props]("CalQueueTabContent")
    .stateless
    .render_P { p =>
      TabPane(active = p.isActive,
              as = As.Segment(Segment(attached = SegmentAttached.Attached, secondary = true)),
              clazz = SeqexecStyles.tabSegment
      )(
        dataTab := "daycal",
        React
          .Fragment(
            <.div(SeqexecStyles.TabControls,
                  p.dayCalConnectOps(_() match {
                    case Some(x) => CalQueueToolbar(CalibrationQueueId, x)
                    case _       => <.div()
                  }).when(p.canOperate)
            ),
            <.div(SeqexecStyles.TabTable,
                  p.dayCalConnect(_() match {
                    case Some(x) =>
                      CalQueueTable(CalibrationQueueId, x)
                    case _       => defaultContent
                  }).when(p.isActive)
            )
          )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] =
    component(p)
}
