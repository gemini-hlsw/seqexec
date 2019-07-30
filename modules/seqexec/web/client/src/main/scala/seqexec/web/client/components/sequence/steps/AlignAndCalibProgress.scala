// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.Show
import cats.implicits._
import gem.util.Enumerated
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.MonocleReact._
import monocle.macros.Lenses
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui.elements.progress.Progress
import seqexec.web.client.model.AlignAndCalibStep
import seqexec.web.client.model.AlignAndCalibStep._
import seqexec.web.client.model.StepItems.StepStateSnapshot
import seqexec.web.client.reusability._
import scala.math.max

object ACProgressBar {
  final case class Props(step: AlignAndCalibStep, state: StepStateSnapshot)

  @Lenses
  final case class State(counter: Int, msg: String)

  object State {
    def initialStateFromProps(p: Props): State = p.step match {
      case Done => State(0, "Preparing...")
      case x    => State(stepsOrdering.indexOf(x), x.show)
    }
  }

  implicit val stepReuse: Reusability[StepStateSnapshot] = Reusability.never
  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val stateReuse: Reusability[State] = Reusability.derive[State]

  val enum = Enumerated[AlignAndCalibStep]
  implicit val showACS: Show[AlignAndCalibStep] = Show.show {
    case NoAction           => ""
    case StartGuiding       => "Start Guiding"
    case StopGuiding        => "Stop Guiding"
    case SuperContOff       => "SuperContinuum Off"
    case OMSSEntShutterOff  => "Entrance shutter closed"
    case CalExistShutterOff => "CAL Exit shutter closed"
    case ArtSourceDeploy    => "Artificial Source deploy"
    case AoDarks            => "Take AO Darks"
    case SuperContOn        => "SuperContinumm On"
    case CalFlags           => "Set CAL flags"
    case Twt2Lens           => "TWT to Lens"
    case CalExitShutterOn   => "CAL Exit shutten opened"
    case ArtSourceExtract   => "Artificial Source extract"
    case OMSSEntShutterOn   => "Entrance shutter opened"
    case InputFoldTracking  => "Input fold tracking"
    case Done               => "Done"
  }

  // The typical AC sequence is like this
  // We use it to guestimate the location when the component is created
  val stepsOrdering = List(
    SuperContOff,
    OMSSEntShutterOff,
    CalExistShutterOff,
    ArtSourceDeploy,
    AoDarks,
    SuperContOn,
    CalFlags,
    Twt2Lens,
    StartGuiding,
    StopGuiding,
    SuperContOff,
    ArtSourceExtract,
    OMSSEntShutterOn,
    InputFoldTracking,
    Done
  )

  private val component = ScalaComponent
    .builder[Props]("ACProgressBar")
    .initialStateFromProps(State.initialStateFromProps)
    .render_PS { (p, s) =>
      val isInError = !p.state.isACRunning && p.state.isACInError
      val msg = if (isInError) "Error" else s.msg
      Progress(Progress.Props(
        s"Align and Calib: $msg",
        total       = enum.all.length.toLong - 1,
        value       = max(0, s.counter.toLong),
        color       = if (isInError) "red".some else "green".some,
        progressCls = List(SeqexecStyles.observationProgressBar),
        barCls      = List(SeqexecStyles.observationBar),
        labelCls    = List(SeqexecStyles.observationLabel)
      ))
    }
    .componentWillReceiveProps(x =>
      x.modStateL(State.counter)(_ + 1) >> x.setStateL(State.msg)(x.nextProps.step.show))
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}

/**
  * Component to wrap the progress bar
  */
object AlignAndCalibProgress {
  final case class Props(state: StepStateSnapshot) {
    protected[steps] val connect =
      SeqexecCircuit.connect(SeqexecCircuit.acProgressRW)
  }

  implicit val stepReuse: Reusability[StepStateSnapshot] = Reusability.never
  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("AlignAndCalibProgress")
    .stateless
    .render_P(p =>
        p.connect{s =>
          ACProgressBar(ACProgressBar.Props(s(), p.state))
        }
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
