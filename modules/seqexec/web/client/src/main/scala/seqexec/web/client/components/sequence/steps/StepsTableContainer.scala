// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import diode.react.ReactConnectProxy
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.CatsReact
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.CatsReact._
import seqexec.web.client.components.sequence.toolbars.SequenceDefaultToolbar
import seqexec.web.client.components.sequence.toolbars.StepConfigToolbar
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.reusability._

object StepsTableContainer {
  final case class Props(router:        RouterCtl[SeqexecPages],
                         statusAndStep: StatusAndStepFocus) {
    val stepsConnect: ReactConnectProxy[StepsTableAndStatusFocus] =
      SeqexecCircuit.connect(
        SeqexecCircuit.stepsTableReader(statusAndStep.obsId))
  }
  final case class State(nextStepToRun: Int)

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.statusAndStep)
  implicit val stateReuse: Reusability[State] = Reusability.derive[State]

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): CatsReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB

  def toolbar(p: Props): VdomElement = {
    val canOperate          = p.statusAndStep.canOperate
    val stepConfigDisplayed = p.statusAndStep.stepConfigDisplayed.isDefined
    val isPreview           = p.statusAndStep.isPreview
    val showDefault         = canOperate && !stepConfigDisplayed && !isPreview

    <.div(
      SequenceDefaultToolbar(
        SequenceDefaultToolbar.Props(p.statusAndStep.obsId)).when(showDefault),
      p.statusAndStep.stepConfigDisplayed
        .map { s =>
          StepConfigToolbar(
            StepConfigToolbar.Props(p.router,
                                    p.statusAndStep.instrument,
                                    p.statusAndStep.obsId,
                                    s,
                                    p.statusAndStep.totalSteps,
                                    isPreview)).when(stepConfigDisplayed)
        }
        .getOrElse(TagMod.empty)
    )
  }

  private val component = ScalaComponent
    .builder[Props]("StepsTableContainer")
    .initialState(State(0))
    .renderP { ($, p) =>
      <.div(
        ^.height := "100%",
        toolbar(p),
        p.stepsConnect(
          r =>
            StepsTable(
              StepsTable.Props(p.router,
                               p.statusAndStep.canOperate,
                               r(),
                               x => $.runState(updateStepToRun(x)))))
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, State, Unit] =
    component(p)
}
