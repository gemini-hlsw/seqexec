// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import react.common._
import react.semanticui.modules.progress.Progress
import react.semanticui.colors._
import seqexec.model.dhs.ImageFileId
import seqexec.model.{ ObservationProgress, ObserveStage, StepId }
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.reusability._
import scala.math.max

trait ProgressLabel {
  def label(
    fileId:          String,
    remainingMillis: Option[Int],
    stopping:        Boolean,
    paused:          Boolean,
    stage:           ObserveStage
  ): String = {
    val durationStr = remainingMillis.foldMap { millis =>
      val remainingSecs = millis / 1000
      val remainingStr  = if (remainingSecs > 1) s"$remainingSecs seconds" else "1 second"
      s" - $remainingStr left"
    }
    val stageStr =
      stage match {
        case ObserveStage.Preparing  => "Preparing".some
        case ObserveStage.ReadingOut => "Reading out...".some
        case _                       => None
      }

    if (paused) s"$fileId - Paused$durationStr"
    else if (stopping) s"$fileId - Stopping - Reading out..."
    else
      stageStr match {
        case Some(stage) => s"$fileId - $stage"
        case _ =>
          remainingMillis.fold(fileId) { millis =>
            if (millis > 0) s"$fileId$durationStr" else s"$fileId - Reading out..."
          }
      }
  }
}

/**
  * Component to wrap the progress bar
  */
final case class ObservationProgressBar(
  obsId:    Observation.Id,
  stepId:   StepId,
  fileId:   ImageFileId,
  stopping: Boolean,
  paused:   Boolean
) extends ReactProps[ObservationProgressBar](ObservationProgressBar.component) {

  protected[steps] val connect =
    SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader[ObservationProgress](obsId, stepId))
}

object ObservationProgressBar {
  type Props = ObservationProgressBar

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  val component = ScalaComponent
    .builder[Props]("ObservationProgressDisplay")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.observationProgressRow,
        p.connect(proxy =>
          proxy() match {
            case Some(ObservationProgress(_, _, total, remaining, stage)) =>
              SmoothObservationProgressBar(
                p.fileId,
                total.toMilliseconds.toInt,
                total.toMilliseconds.toInt - max(0, remaining.toMilliseconds.toInt),
                p.stopping,
                p.paused,
                stage
              )
            case _ =>
              val msg = if (p.paused) s"${p.fileId} - Paused" else p.fileId

              React.Fragment(
                Progress(
                  indicating = true,
                  total      = 100,
                  value      = 0,
                  color      = Blue,
                  clazz      = SeqexecStyles.observationProgressBar
                )(msg)
              )
          }
        )
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build
}
