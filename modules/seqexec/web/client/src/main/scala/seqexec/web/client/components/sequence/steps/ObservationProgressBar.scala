// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.extra.TimerSupport
import react.common.implicits._
import seqexec.model.dhs.ImageFileId
import seqexec.model.ObservationProgress
import seqexec.model.StepId
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui.elements.progress.Progress
import web.client.ReactProps

import scala.math.max

trait ProgressLabel {
  def label(
    fileId:          String,
    remainingMillis: Int,
    stopping:        Boolean,
    paused:          Boolean
  ): String = {
    val remainingSecs = remainingMillis / 1000
    val durationStr = if (remainingSecs > 1) s"$remainingSecs seconds" else "1 second"

    if (paused) s"$fileId - Paused - $durationStr left"
      else if (stopping) s"$fileId - Stopping - $durationStr left"
      else if (remainingSecs > 0) s"$fileId - $durationStr left"
      else s"$fileId - Reading out..."
  }
}

final case class SmoothObservationProgressBar(
  fileId:   String,
  total:    Int,
  value:    Int,
  stopping: Boolean,
  paused:   Boolean
) extends SmoothProgressBarProps {
  @inline def render: VdomElement = SmoothObservationProgressBar.component(this)

  override val maxValue = total
}

object SmoothObservationProgressBar
  extends SmoothProgressBar[SmoothObservationProgressBar] with ProgressLabel {
  type Props = SmoothObservationProgressBar

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("SmoothProgressBar")
    .initialStateFromProps(State.fromProps)
    .backend(x => new Backend(x))
    .render_PS { (p, s) =>
      val remainingMillis = s.maxValue - s.value

      Progress(
        label       = label(p.fileId, remainingMillis, p.stopping, p.paused),
        total       = p.total,
        value       = s.value,
        color       = "blue".some,
        progressCls = List(SeqexecStyles.observationProgressBar),
        barCls      = List(SeqexecStyles.observationBar),
        labelCls    = List(SeqexecStyles.observationLabel)
      )
    }
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps(x =>
      x.backend.newStateFromProps(x.currentProps, x.nextProps))
    .configure(TimerSupport.install)
    .configure(Reusability.shouldComponentUpdate)
    .build
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
) extends ReactProps {
  @inline def render: VdomElement = ObservationProgressBar.component(this)

  protected[steps] val connect =
    SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader[ObservationProgress](obsId, stepId))
}

object ObservationProgressBar {
  type Props = ObservationProgressBar

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("ObservationProgressDisplay")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.observationProgressRow,
        p.connect(proxy =>
          proxy() match {
            case Some(ObservationProgress(_, _, total, remaining)) =>
              SmoothObservationProgressBar(
                p.fileId,
                total.toMilliseconds.toInt,
                total.toMilliseconds.toInt - max(0, remaining.toMilliseconds.toInt),
                p.stopping,
                p.paused)
            case _ =>
              val msg = if (p.paused) s"${p.fileId} - Paused" else p.fileId

              Progress(
                msg,
                total       = 100,
                value       = 0,
                color       = "blue".some,
                progressCls = List(SeqexecStyles.observationProgressBar),
                barCls      = List(SeqexecStyles.observationBar),
                labelCls    = List(SeqexecStyles.observationLabel)
              )
        })
    ))
    .configure(Reusability.shouldComponentUpdate)
    .build
}
