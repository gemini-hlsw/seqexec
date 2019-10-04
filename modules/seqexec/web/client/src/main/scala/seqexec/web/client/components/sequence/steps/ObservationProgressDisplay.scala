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

final case class SmoothProgressDisplay(
  fileId:   String,
  total:    Int,
  value:    Int,
  stopping: Boolean,
  paused:   Boolean,
  hideBar:  Boolean
) extends SmoothProgressBarProps {
  @inline def render: VdomElement = SmoothProgressDisplay.component(this)

  override val maxValue = total
}

object SmoothProgressDisplay extends SmoothProgressBar[SmoothProgressDisplay] {
  type Props = SmoothProgressDisplay

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("SmoothProgressBar")
    .initialStateFromProps(State.fromProps)
    .backend(x => new Backend(x))
    .render_PS { (p, s) =>
      val remaining   = (s.maxValue - s.value) / 1000
      val durationStr = if (remaining > 1) s"$remaining seconds" else "1 second"
      val label =
        if (p.paused) s"${p.fileId} - Paused - $durationStr left"
        else if (p.stopping) s"${p.fileId} - Stopping - $durationStr left"
        else if (remaining > 0) s"${p.fileId} - $durationStr left"
        else s"${p.fileId} - Reading out..."

      if(p.hideBar)
        <.div(
          SeqexecStyles.specialStateLabel,
          SeqexecStyles.progressMessage,
          label
        )
      else
        Progress(Progress.Props(
          label       = label,
          total       = p.total,
          value       = s.value,
          color       = "blue".some,
          progressCls = List(SeqexecStyles.observationProgressBar),
          barCls      = List(SeqexecStyles.observationBar),
          labelCls    = List(SeqexecStyles.observationLabel)
        ))
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
final case class ObservationProgressDisplay(
  obsId:    Observation.Id,
  stepId:   StepId,
  fileId:   ImageFileId,
  stopping: Boolean,
  paused:   Boolean,
  hideBar:  Boolean
) extends ReactProps {
  @inline def render: VdomElement = ObservationProgressDisplay.component(this)

  protected[steps] val connect =
    SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader(obsId, stepId))
}

object ObservationProgressDisplay {
  type Props = ObservationProgressDisplay

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
              SmoothProgressDisplay(
                p.fileId,
                total.toMilliseconds.toInt,
                total.toMilliseconds.toInt - max(0, remaining.toMilliseconds.toInt),
                p.stopping,
                p.paused,
                p.hideBar)
            case _ =>
              val msg = if (p.paused) s"${p.fileId} - Paused" else p.fileId

              if(p.hideBar)
                <.div(
                  SeqexecStyles.specialStateLabel,
                  SeqexecStyles.progressMessage,
                  msg
                )
              else
                Progress(Progress.Props(
                  msg,
                  total       = 100,
                  value       = 0,
                  color       = "blue".some,
                  progressCls = List(SeqexecStyles.observationProgressBar),
                  barCls      = List(SeqexecStyles.observationBar),
                  labelCls    = List(SeqexecStyles.observationLabel)
                ))
        })
    ))
    .configure(Reusability.shouldComponentUpdate)
    .build
}
