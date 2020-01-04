// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import gem.Observation
import seqexec.model.{ObservationProgress, StepId}
import seqexec.web.client.circuit.SeqexecCircuit
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._

case class BiasStatus(
  obsId: Observation.Id,
  stepId: StepId,
  fileId: String,
  stopping: Boolean,
  paused: Boolean
) extends ReactProps {
  @inline def render: VdomElement = BiasStatus.component(this)

  protected[steps] val connect =
    SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader[ObservationProgress](obsId, stepId))
}

object BiasStatus extends ProgressLabel {
  type Props = BiasStatus

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("BiasStatus")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.specialStateLabel,
        p.connect(proxy =>
          <.span(
            proxy() match {
              case Some(ObservationProgress(_, _, _, _, stage)) =>
                label(p.fileId, None, p.stopping, p.paused, stage)
              case _ =>
                if (p.paused) s"${p.fileId} - Paused" else p.fileId
            }
          )
        )
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build
}
