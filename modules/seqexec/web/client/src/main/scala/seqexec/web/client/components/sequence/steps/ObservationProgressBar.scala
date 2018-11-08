// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.TimerSupport
import gem.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.model.ObservationProgress
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui.elements.progress.Progress
import scala.concurrent.duration._
import scala.math.max
import scala.math.min
import web.client.style._

object SmoothProgressBar {
  val periodUpdate: Int = 100
  final case class Props(label: String, total: Long, value: Long)
  final case class State(total: Long, value:   Long)

  object State {
    def fromProps(p: Props): State = State(p.total, p.value)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val stateReuse: Reusability[State] = Reusability.never

  class Backend(b: BackendScope[Props, State]) extends TimerSupport {
    def setupTimer: Callback =
      setInterval(
        b.modState(x => x.copy(value = min(x.total, x.value + periodUpdate))),
        periodUpdate.millisecond)
    def newProps(p: Props): Callback = b.setState(State.fromProps(p))
  }

  private val component = ScalaComponent
    .builder[Props]("ObservationProgressBar")
    .initialStateFromProps(State.fromProps)
    .backend(x => new Backend(x))
    .render_PS((p, s) =>
      Progress(Progress.Props(
        label       = p.label,
        total       = p.total,
        value       = s.value,
        indicating  = s.value < s.total,
        progress    = true,
        progressCls = List(SeqexecStyles.observationProgressBar),
        barCls      = List(SeqexecStyles.observationBar),
        labelCls    = List(SeqexecStyles.observationLabel)
      )))
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps(x => x.backend.newProps(x.nextProps))
    .configure(TimerSupport.install)
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, State, Backend] = component(p)
}

/**
  * Component to wrap the progress bar
  */
object ObservationProgressBar {
  final case class Props(obsId: Observation.Id, fileId: ImageFileId) {
    protected[steps] val connect =
      SeqexecCircuit.connect(SeqexecCircuit.obsProgressReader(obsId))
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
            case Some(ObservationProgress(_, r, t)) =>
              val label =
                if (t.millis > 0) p.fileId else s"${p.fileId} - Completing..."
              SmoothProgressBar(
                SmoothProgressBar.Props(label,
                                        r.millis,
                                        r.millis - max(0, t.millis)))
            case _ =>
              Progress(Progress.Props(
                p.fileId,
                total       = 100,
                value       = 0,
                indicating  = false,
                progress    = true,
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
