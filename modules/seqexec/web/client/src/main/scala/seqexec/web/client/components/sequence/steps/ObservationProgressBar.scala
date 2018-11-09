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
import java.time.Duration
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
  final case class Props(fileId: String,
                         total:  Long,
                         value:  Long,
                         paused: Boolean)
  final case class State(total:  Long, value: Long, skipStep: Boolean)

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
        val next = s.value + periodUpdate
        b.modState(x => x.copy(value = min(p.total, next)))
          .when(!s.skipStep && !p.paused) *>
          b.modState(x => x.copy(skipStep = false)) *>
          Callback.empty
    }
  }

  def encodeDuration(duration: Duration): String = {
    val oneMinute  = 60
    val twoMinutes = oneMinute * 2
    val oneHour    = oneMinute * 60
    val twoHours   = oneHour * 2
    val oneDay     = oneHour * 24

    def toString(result: List[String], seconds: Long): List[String] =
      seconds match {
        case seconds if seconds <= 0 =>
          List.empty[String]
        case seconds if seconds === 1 =>
          result ::: List(s"${seconds} second")
        case seconds if seconds < oneMinute =>
          result ::: List(s"${seconds} seconds")
        case seconds if seconds >= oneMinute && seconds < twoMinutes =>
          s"${seconds / oneMinute} minute" :: toString(result,
                                                       seconds % oneMinute)
        case seconds if seconds >= oneMinute && seconds < oneHour =>
          s"${seconds / oneMinute} minutes" :: toString(result,
                                                        seconds % oneMinute)
        case seconds if seconds >= oneHour && seconds < twoHours =>
          s"${seconds / oneHour} hour" :: toString(result, seconds % oneHour)
        case seconds if seconds >= twoHours && seconds < oneDay =>
          s"${seconds / oneHour} hours" :: toString(result, seconds % oneHour)
      }

    toString(List.empty[String], duration.getSeconds).mkString(", ")
  }

  private val component = ScalaComponent
    .builder[Props]("ObservationProgressBar")
    .initialStateFromProps(State.fromProps)
    .backend(x => new Backend(x))
    .render_PS { (p, s) =>
      val remaining   = Duration.ofMillis(s.total - s.value)
      val durationStr = encodeDuration(remaining)
      val remainingStr =
        if (durationStr.isEmpty) " - Completing..." else s" - $durationStr"
      val label =
        if (p.paused) s"${p.fileId} - Paused - $durationStr"
        else if (s.value > 0) s"${p.fileId}$remainingStr"
        else s"${p.fileId} - Completing..."

      Progress(Progress.Props(
        label       = label,
        total       = p.total,
        value       = s.value,
        indicating  = s.value < s.total,
        progress    = true,
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
                         fileId: ImageFileId,
                         paused: Boolean) {
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
