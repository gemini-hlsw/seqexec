// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.queue

import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import react.common.implicits._
import seqexec.model.CalibrationQueueId
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.message.IconMessage
import seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.model.SectionVisibilityState.SectionClosed
import seqexec.web.client.model.SectionVisibilityState.SectionOpen
import seqexec.web.client.model.SectionVisibilityState
import seqexec.web.client.model.TabSelected
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._

/**
  * Content of the queue tab
  */
object CalQueueTabContent {
  final case class Props(canOperate:   Boolean,
                         active:       TabSelected,
                         logDisplayed: SectionVisibilityState) {
    protected[queue] val dayCalConnectOps =
      SeqexecCircuit.connect(
        SeqexecCircuit.calQueueControlReader(CalibrationQueueId))
    protected[queue] val dayCalConnect =
      SeqexecCircuit.connect(SeqexecCircuit.calQueueReader(CalibrationQueueId))
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val defaultContent = IconMessage(
    IconMessage
      .Props(IconInbox, Some("Work in progress"), IconMessage.Style.Warning))

  private val component = ScalaComponent
    .builder[Props]("CalQueueTabContent")
    .stateless
    .render_P { p =>
      <.div(
        ^.cls := "ui attached secondary segment tab",
        ^.classSet(
          "active" -> (p.active === TabSelected.Selected)
        ),
        dataTab := "daycal",
        SeqexecStyles.tabSegment,
        SeqexecStyles.tabSegmentLogShown
          .when(p.logDisplayed === SectionOpen),
        SeqexecStyles.tabSegmentLogHidden
          .when(p.logDisplayed === SectionClosed),
        <.div(
          ^.height := "100%",
          p.dayCalConnectOps(_() match {
              case Some(x) => CalQueueToolbar.Props(CalibrationQueueId, x).cmp
              case _       => <.div()
            })
            .when(p.canOperate),
          p.dayCalConnect(_() match {
            case Some(x) =>
              <.div(
                ^.height := "100%",
                CalQueueTable(CalQueueTable.Props(CalibrationQueueId, x))
              )
            case _ => defaultContent
          })
        ).when(p.active === TabSelected.Selected)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] =
    component(p)
}
