// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model.Step
import edu.gemini.seqexec.web.client.circuit.{ClientStatus, StepsTableFocus}
import edu.gemini.seqexec.web.client.ModelOps._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import scalacss.ScalaCssReact._

import scalaz.syntax.show._

/**
  * Component to display the step state and control
  */
object StepProgressCell {
  final case class Props(clientStatus: ClientStatus, focus: StepsTableFocus, step: Step) {
    val steps: List[Step] = focus.steps
  }
  def stepDisplay(focus: StepsTableFocus, step: Step): VdomNode =
    (focus.state, step.status) match {
      // case (s, StepState.Running | StepState.Paused)     => controlButtons(status.isLogged, p, step)
      // case (_, StepState.Failed(msg))                    => stepInError(status.isLogged, isPartiallyExecuted(p), msg)
      case (_, _) if step.skip                           => <.p("Skipped")
      case (_, _)                                        => <.p(step.status.shows)
    }

  private val component = ScalaComponent.builder[Props]("StepProgressCell")
    .stateless
    .render_P { p =>
      <.div( // Column state/progress type
        SeqexecStyles.leftCell,
        stepDisplay(p.focus, p.step)
      )
    }.build

  def apply(i: Props): Unmounted[Props, Unit, Unit] = component(i)
}
