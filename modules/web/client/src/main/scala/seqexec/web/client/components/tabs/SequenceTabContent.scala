// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.tabs

import cats.syntax.all._
import diode.react.ReactConnectProxy
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.As
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentAttached
import react.semanticui.modules.tab.TabPane
import seqexec.web.client.circuit._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.sequence.steps.StepsTable
import seqexec.web.client.components.sequence.toolbars.SequenceDefaultToolbar
import seqexec.web.client.components.sequence.toolbars.StepConfigToolbar
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui._

/**
 * Content of a single tab with a sequence
 */
final case class SequenceTabContent(
  router:  RouterCtl[SeqexecPages],
  content: SequenceTabContentFocus
) extends ReactProps[SequenceTabContent](SequenceTabContent.component) {

  val stepsConnect: ReactConnectProxy[StepsTableAndStatusFocus] =
    SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(content.id))
}

object SequenceTabContent {
  type Props = SequenceTabContent

  implicit val stcfReuse: Reusability[SequenceTabContentFocus] =
    Reusability.derive[SequenceTabContentFocus]
  implicit val propsReuse: Reusability[Props]                  = Reusability.by(_.content)

  private def toolbar(p: Props) =
    p.content.tableType match {
      case StepsTableTypeSelection.StepsTableSelected         =>
        SequenceDefaultToolbar(p.content.id)
          .when(p.content.canOperate && !p.content.isPreview)
      case StepsTableTypeSelection.StepConfigTableSelected(s) =>
        StepConfigToolbar(p.router,
                          p.content.instrument,
                          p.content.id,
                          s,
                          p.content.totalSteps,
                          p.content.isPreview
        ): TagMod
    }

  def stepsTable(p: Props): VdomElement =
    p.content.tableType match {
      case StepsTableTypeSelection.StepsTableSelected =>
        p.stepsConnect { x =>
          StepsTable(p.router, p.content.canOperate, x())
        }

      case StepsTableTypeSelection.StepConfigTableSelected(i) =>
        p.stepsConnect { x =>
          val focus = x()

          focus.stepsTable
            .foldMap(_.steps)
            .lift(i)
            .map { steps =>
              val hs = focus.configTableState
              <.div(
                ^.height := "100%",
                StepConfigTable(steps, hs)
              )
            }
            .getOrElse(<.div())
        }
    }

  protected val component = ScalaComponent
    .builder[Props]
    .stateless
    .render_P { p =>
      val instrument = p.content.instrument
      TabPane(
        active = p.content.isActive,
        as = As.Segment(
          Segment(compact = true, attached = SegmentAttached.Attached, secondary = true)
        ),
        clazz = SeqexecStyles.tabSegment
      )(
        dataTab := instrument.show,
        <.div(SeqexecStyles.TabControls, toolbar(p).when(p.content.canOperate)),
        <.div(SeqexecStyles.TabTable, stepsTable(p).when(p.content.isActive))
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
