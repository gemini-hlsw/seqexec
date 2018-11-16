// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.TimerSupport
import monocle.macros.Lenses
import seqexec.model.dhs.ImageFileId
import seqexec.model.ObservationProgress
import seqexec.model.StepId
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui.elements.progress.Progress
import scala.concurrent.duration._
import scala.math.max
import scala.math.min
import web.client.style._

object SmoothProgressBar {
  val periodUpdate: Int = 50
  // This depends on the server side frequency of updates
  val remoteUpdatePeriod: Int = 1000

  final case class Props(fileId: String,
                         total:  Long,
                         value:  Long,
                         paused: Boolean)

  @Lenses
  final case class State(total: Long, value: Long, skipStep: Boolean)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object State {
    def fromProps(p: Props): State = State(p.total, p.value, false)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val stateReuse: Reusability[State] = Reusability.derive[State]

  class Backend(b: BackendScope[Props, State]) extends TimerSupport {
    def setupTimer: Callback =
      setInterval(tickTotal, periodUpdate.millisecond)

    def newStateFromProps(p: Props): Callback =
      b.modState(s => State(p.total, p.value, s.value > p.value))

    def tickTotal: Callback = b.props.zip(b.state) >>= {
      case (p, s) =>
        val next = min(s.value + periodUpdate, p.value + remoteUpdatePeriod)
        b.modState(State.value.set(min(p.total, next)))
          .when(!s.skipStep && !p.paused) *>
          b.modState(State.skipStep.set(false)) *>
          Callback.empty
    }
  }

  private val component = ScalaComponent
    .builder[Props]("ObservationProgressBar")
    .initialStateFromProps(State.fromProps)
    .backend(x => new Backend(x))
    .render_PS { (p, s) =>
      val remaining   = (s.total - s.value) / 1000
      val durationStr = if (remaining > 1) s"$remaining seconds" else "1 second"
      val label =
        if (p.paused) s"${p.fileId} - Paused - $durationStr left"
        else if (remaining > 0) s"${p.fileId} - $durationStr left"
        else s"${p.fileId} - Reading out..."

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
    .componentWillReceiveProps(x => x.backend.newStateFromProps(x.nextProps))
    .configure(TimerSupport.install)
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, State, Backend] = component(p)
}

/**
  * Component to wrap the progress bar
  */
object ObservationProgressBar {
  final case class Props(obsId:  Observation.Id,
                         stepId: StepId,
                         fileId: ImageFileId,
                         paused: Boolean) {
    protected[steps] val connect =
      SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader(obsId, stepId))
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("ObservationProgressBar")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.observationProgressRow,
        p.connect(x =>
          x() match {
            case Some(ObservationProgress(_, _, r, t)) =>
              SmoothProgressBar(
                SmoothProgressBar.Props(p.fileId,
                                        r.millis,
                                        r.millis - max(0, t.millis),
                                        p.paused))
            case _ =>
              Progress(Progress.Props(
                if (p.paused) s"${p.fileId} - Paused" else p.fileId,
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

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
