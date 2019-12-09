// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import diode.react.ReactConnectProxy
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
import react.common._
import react.common.implicits._
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.model.SectionVisibilityState.SectionClosed
import seqexec.web.client.model.SectionVisibilityState.SectionOpen
import seqexec.web.client.semanticui.{Size => _, _}
import seqexec.web.client.components.sequence.toolbars.SequenceDefaultToolbar
import seqexec.web.client.components.sequence.toolbars.StepConfigToolbar
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._

/**
  * Content of a single tab with a sequence
  */
final case class SequenceTabContent(
  router:  RouterCtl[SeqexecPages],
  content: SequenceTabContentFocus
) extends ReactProps {
  @inline def render: VdomElement = SequenceTabContent.component(this)

  val stepsConnect: ReactConnectProxy[StepsTableAndStatusFocus] =
    SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(content.id))
}

object SequenceTabContent {
  type Props = SequenceTabContent

  implicit val stcfReuse: Reusability[SequenceTabContentFocus] =
    Reusability.derive[SequenceTabContentFocus]
  implicit val propsReuse: Reusability[Props] = Reusability.by(_.content)

  private def toolbar(p: Props) =
    p.content.tableType match {
      case StepsTableTypeSelection.StepsTableSelected =>
        SequenceDefaultToolbar(SequenceDefaultToolbar.Props(p.content.id))
          .when(p.content.canOperate && !p.content.isPreview)
      case StepsTableTypeSelection.StepConfigTableSelected(s) =>
        StepConfigToolbar(
          StepConfigToolbar
            .Props(p.router,
                   p.content.instrument,
                   p.content.id,
                   s,
                   p.content.totalSteps,
                   p.content.isPreview)): TagMod
    }

  def stepsTable(p: Props): VdomElement =
    p.content.tableType match {
      case StepsTableTypeSelection.StepsTableSelected =>
        p.stepsConnect { x =>
          <.div(
            ^.height := "100%",
            StepsTable(p.router, p.content.canOperate, x())
          )
        }

      case StepsTableTypeSelection.StepConfigTableSelected(i) =>
        p.stepsConnect { x =>
          val focus = x()

          focus.stepsTable.foldMap(_.steps).lift(i).map { steps =>
            val hs = focus.configTableState
            <.div(
              ^.height := "100%",
              StepConfigTable(steps, hs)
            )
          }.getOrElse(<.div())
        }
    }

  protected val component = ScalaComponent
    .builder[Props]("SequenceTabContent")
    .stateless
    .render_P { p =>
      val canOperate   = p.content.canOperate
      val instrument   = p.content.instrument
      val logDisplayed = p.content.logDisplayed

      <.div(
        ^.cls := "ui attached secondary segment tab",
        ^.classSet(
          "active" -> p.content.isActive
        ),
        dataTab := instrument.show,
        SeqexecStyles.tabSegment.when(canOperate),
        SeqexecStyles.tabSegmentLogShown
          .when(canOperate && logDisplayed === SectionOpen),
        SeqexecStyles.tabSegmentLogHidden
          .when(canOperate && logDisplayed === SectionClosed),
        SeqexecStyles.tabSegmentUnauth.when(!canOperate),
        SeqexecStyles.tabSegmentLogShownUnauth
          .when(!canOperate && logDisplayed === SectionOpen),
        SeqexecStyles.tabSegmentLogHiddenUnauth
          .when(!canOperate && logDisplayed === SectionClosed),
        <.div(
          ^.height := "100%",
          toolbar(p),
          stepsTable(p)
        ).when(p.content.isActive)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
