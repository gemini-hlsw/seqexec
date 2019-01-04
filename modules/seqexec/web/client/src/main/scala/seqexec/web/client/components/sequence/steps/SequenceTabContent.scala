// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import diode.react.ReactConnectProxy
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
import seqexec.model.Step
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.model.SectionClosed
import seqexec.web.client.model.SectionOpen
import seqexec.web.client.model.TabSelected
import seqexec.web.client.semanticui.{ Size => _, _ }
import seqexec.web.client.components.sequence.toolbars.SequenceDefaultToolbar
import seqexec.web.client.components.sequence.toolbars.StepConfigToolbar
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._
import react.virtualized._
import web.client.style._

/**
  * Content of a single tab with a sequence
  */
object SequenceTabContent {
  final case class Props(router: RouterCtl[SeqexecPages],
                         p:      SequenceTabContentFocus) {

    val stepsConnect: ReactConnectProxy[StepsTableAndStatusFocus] =
      SeqexecCircuit.connect(SeqexecCircuit.stepsTableReader(p.id))
  }

  implicit val stcfReuse: Reusability[SequenceTabContentFocus] =
    Reusability.derive[SequenceTabContentFocus]
  implicit val propsReuse: Reusability[Props] = Reusability.by(_.p)

  def toolbar(router: RouterCtl[SeqexecPages],
              p:      SequenceTabContentFocus): VdomElement =
    p.tableType match {
      case StepsTableTypeSelection.StepsTableSelected
          if p.canOperate && !p.isPreview =>
        SequenceDefaultToolbar(SequenceDefaultToolbar.Props(p.id))
      case StepsTableTypeSelection.StepConfigTableSelected(s) =>
        StepConfigToolbar(
          StepConfigToolbar
            .Props(router, p.instrument, p.id, s, p.totalSteps, p.isPreview))
      case _ =>
        <.div()
    }

  def stepsTable(p: Props, s: Size): VdomElement =
    p.p.tableType match {
      case StepsTableTypeSelection.StepsTableSelected =>
        p.stepsConnect(x =>
          StepsTable(StepsTable.Props(p.router, p.p.canOperate, x(), s)))
      case StepsTableTypeSelection.StepConfigTableSelected(i) =>
        p.stepsConnect { x =>
          val focus = x()

          val steps =
            focus.stepsTable.foldMap(_.steps).lift(i).getOrElse(Step.Zero)
          val hs = focus.configTableState
          StepConfigTable(StepConfigTable.Props(steps, s, hs))
        }
    }

  private val component = ScalaComponent
    .builder[Props]("SequenceTabContent")
    .stateless
    .render_P { p =>
      val canOperate   = p.p.canOperate
      val instrument   = p.p.instrument
      val active       = p.p.active
      val logDisplayed = p.p.logDisplayed

      <.div(
        ^.cls := "ui attached secondary segment tab",
        ^.classSet(
          "active" -> (active === TabSelected.Selected)
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
          toolbar(p.router, p.p),
          AutoSizer(AutoSizer.props(s => stepsTable(p, s)))
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] =
    component(p)
}
