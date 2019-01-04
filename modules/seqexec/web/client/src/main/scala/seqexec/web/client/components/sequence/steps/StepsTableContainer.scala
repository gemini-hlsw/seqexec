// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

// import diode.react.ReactConnectProxy
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
// import seqexec.web.client.components.sequence.toolbars.SequenceDefaultToolbar
// import seqexec.web.client.components.sequence.toolbars.StepConfigToolbar
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.reusability._

object StepsTableContainer {
  final case class Props(router:         RouterCtl[SeqexecPages],
                         stepsTableType: StepsTableTypeSelection) {
    // val stepsConnect: ReactConnectProxy[StepsTableAndStatusFocus] =
    //   SeqexecCircuit.connect(
    //     SeqexecCircuit.stepsTableReader(statusAndStep.obsId))
  }

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.stepsTableType)

  // def toolbar(p: Props): VdomElement = {
  //   val canOperate          = p.statusAndStep.canOperate
  //   val stepConfigDisplayed = p.statusAndStep.stepConfigDisplayed.isDefined
  //   val isPreview           = p.statusAndStep.isPreview
  //   val showDefault         = canOperate && !stepConfigDisplayed && !isPreview
  //
  //   <.div(
  //     SequenceDefaultToolbar(
  //       SequenceDefaultToolbar.Props(p.statusAndStep.obsId)).when(showDefault),
  //     p.statusAndStep.stepConfigDisplayed
  //       .map { s =>
  //         StepConfigToolbar(
  //           StepConfigToolbar.Props(p.router,
  //                                   p.statusAndStep.instrument,
  //                                   p.statusAndStep.obsId,
  //                                   s,
  //                                   p.statusAndStep.totalSteps,
  //                                   isPreview)).when(stepConfigDisplayed)
  //       }
  //       .getOrElse(TagMod.empty)
  //   )
  // }

  private val component = ScalaComponent
    .builder[Props]("StepsTableContainer")
    .stateless
    .render_P { _ =>
      <.div(
        ^.height := "100%",
        // toolbar(p),
        // AutoSizer(AutoSizer.props(s =>
        // ))
        // p.stepsConnect(
        //   r =>
        //     StepsTable(
        //       StepsTable.Props(p.router,
        //                        p.statusAndStep.canOperate,
        //                        r())))
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] =
    component(p)
}
