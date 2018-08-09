// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import monocle.macros.Lenses

import cats._
import cats.implicits._
import gem.Observation
import seqexec.model.enum._

@SuppressWarnings(Array("org.wartremover.warts.PublicInference", "org.wartremover.warts.IsInstanceOf"))
// scalastyle:off
object Model {

  type ParamName = String
  type ParamValue = String
  type Parameters = Map[ParamName, ParamValue]
  type StepConfig = Map[SystemName, Parameters]
  implicit val stEq: Eq[StepConfig] = Eq.fromUniversalEquals
  type StepId = Int
  type ObservationName = String
  type TargetName = String
  type ClientID = java.util.UUID
  implicit val clientIdEq: Eq[ClientID] = Eq.fromUniversalEquals
  val DaytimeCalibrationTargetName = "Daytime calibration"

  final case class Operator(value: String)

  object Operator {
    val Zero: Operator = Operator("")
    implicit val equal: Eq[Operator] = Eq.fromUniversalEquals
    implicit val shows: Show[Operator] = Show.show(_.value)
  }

  final case class Observer(value: String)
  object Observer {
    val Zero: Observer = Observer("")
    implicit val equal: Eq[Observer] = Eq.fromUniversalEquals
    implicit val shows: Show[Observer] = Show.show(_.value)
  }

  sealed trait Step {
    val id: StepId
    val config: StepConfig
    val status: StepState
    val breakpoint: Boolean
    val skip: Boolean
    val fileId: Option[dhs.ImageFileId]
  }
  object Step {
    val Zero: Step = StandardStep(id = -1, config = Map.empty, status = StepState.Pending, breakpoint = false, skip = false, fileId = None, configStatus = Nil, observeStatus = ActionStatus.Pending)

    implicit val equal: Eq[Step] =
      Eq.by { x =>
        (x.id, x.config, x.status, x.breakpoint, x.skip, x.fileId)
      }

    implicit class StepOps(val s: Step) extends AnyVal {
      def flipBreakpoint: Step = s match {
        case st: StandardStep => st.copy(breakpoint = !st.breakpoint)
        case st               => st
      }

      def flipSkip: Step = s match {
        case st: StandardStep => st.copy(skip = !st.skip)
        case st               => st
      }

      def file: Option[String] = None

      def canSetBreakpoint(i: Int, firstRunnable: Int): Boolean = s.status match {
        case StepState.Pending | StepState.Skipped | StepState.Paused => i > firstRunnable
        case _                                                        => false
      }

      def canSetSkipmark: Boolean = s.status match {
        case StepState.Pending | StepState.Paused => true
        case _ if hasError                        => true
        case _                                    => false
      }

      def hasError: Boolean = s.status match {
        case StepState.Failed(_) => true
        case _                  => false
      }

      def isObserving: Boolean = s match {
        case StandardStep(_, _, _, _, _, _, _, o) => o === ActionStatus.Running
        case _                                    => false
      }

      def isObservePaused: Boolean = s match {
        case StandardStep(_, _, _, _, _, _, _, o) => o === ActionStatus.Paused
        case _                                    => false
      }

      def isConfiguring: Boolean = s match {
        case StandardStep(_, _, _, _, _, _, c, _) => c.map(_._2).contains(ActionStatus.Running)
        case _                                    => false
      }

      def isFinished: Boolean = s.status === StepState.Completed || s.status === StepState.Skipped

      def wasSkipped: Boolean = s.status === StepState.Skipped

    }
  }

  final case class StandardStep(
    override val id: StepId,
    override val config: StepConfig,
    override val status: StepState,
    override val breakpoint: Boolean,
    override val skip: Boolean,
    override val fileId: Option[dhs.ImageFileId],
    configStatus: List[(Resource, ActionStatus)],
    observeStatus: ActionStatus
  ) extends Step
  object StandardStep {
    implicit val equal: Eq[StandardStep] =
      Eq.by { x =>
        (x.id, x.config, x.status, x.breakpoint, x.skip, x.fileId, x.configStatus, x.observeStatus)
      }
  }
  // Other kinds of Steps to be defined.

  sealed trait SequenceState extends Product with Serializable
  object SequenceState {
    case object Completed         extends SequenceState
    case object Idle              extends SequenceState
    case object Stopped           extends SequenceState
    final case class Running(userStop: Boolean, internalStop: Boolean) extends SequenceState
    object Running {
      val init: Running = Running(userStop = false, internalStop = false)
    }
    final case class Failed(msg: String) extends SequenceState

    // Operations on the sequence state
    implicit class SequenceStateOps(val state: SequenceState) extends AnyVal {
      def internalStopRequested: Boolean = state match {
        case SequenceState.Running(_, true) => true
        case _                              => false
      }

      def isError: Boolean = state match {
        case Failed(_) => true
        case _         => false
      }

      def isInProcess: Boolean = state =!= SequenceState.Idle

      def isRunning: Boolean = state match {
        case Running(_, _) => true
        case _             => false
      }

      def isCompleted: Boolean = state === SequenceState.Completed

      def isIdle: Boolean = state === SequenceState.Idle

      def isStopped: Boolean = state === SequenceState.Stopped

      def userStopRequested: Boolean = state match {
        case SequenceState.Running(true, _) => true
        case _                              => false
      }

    }

    implicit val equal: Eq[SequenceState] = Eq.fromUniversalEquals
  }

  /**
    * Metadata about the sequence required on the exit point
    */
  // TODO Use a proper instrument class
  @Lenses final case class SequenceMetadata(
    instrument: Instrument,
    observer: Option[Observer],
    name: String
  )

  object SequenceMetadata {
    implicit val eq: Eq[SequenceMetadata] =
      Eq.by(x => (x.instrument, x.observer, x.name))
  }

  @Lenses final case class SequenceView (
    id: Observation.Id,
    metadata: SequenceMetadata,
    status: SequenceState,
    steps: List[Step],
    willStopIn: Option[Int]
  )

  object SequenceView {
    implicit val eq: Eq[SequenceView] =
      Eq.by { x =>
        (x.id, x.metadata, x.status, x.steps, x.willStopIn)
      }
  }

  /**
    * Represents a queue with different levels of details. E.g. it could be a list of Ids
    * Or a list of fully hydrated SequenceViews
    */
  final case class SequencesQueue[T](selected: Map[Instrument, Observation.Id], conditions: Conditions, operator: Option[Operator], queue: List[T])

  object SequencesQueue {
    implicit def equal[T: Eq]: Eq[SequencesQueue[T]] =
      Eq.by { x =>
        (x.conditions, x.operator, x.queue)
      }
  }


  sealed trait OffsetAxis {
    val configItem: String
  }
  object OffsetAxis {
    case object AxisP extends OffsetAxis {
      val configItem = "p"
    }
    case object AxisQ extends OffsetAxis {
      val configItem = "q"
    }
    implicit val show: Show[OffsetAxis] = Show.show {
      case AxisP => "p"
      case AxisQ => "q"
    }
  }

  sealed trait Offset {
    val value: Double
  }
  object Offset {
    implicit val equal: Eq[Offset] =
      Eq.by {
        case p: TelescopeOffset.P => Left(p)
        case q: TelescopeOffset.Q => Right(q)
      }

    def Zero(axis: OffsetAxis): Offset = axis match {
      case OffsetAxis.AxisP => TelescopeOffset.P.Zero
      case OffsetAxis.AxisQ => TelescopeOffset.Q.Zero
    }
  }

  // Telescope offsets, roughly based on gem
  final case class TelescopeOffset(p: TelescopeOffset.P, q: TelescopeOffset.Q)
  object TelescopeOffset {
    /** P component of an angular offset.. */
    final case class P(value: Double) extends Offset
    object P {
      val Zero: P = P(0.0)
      implicit val order: Order[P] = Order.by(_.value)

    }
    /** Q component of an angular offset.. */
    final case class Q(value: Double) extends Offset
    object Q {
      val Zero: Q = Q(0.0)
      implicit val order: Order[Q] = Order.by(_.value)

    }
    implicit val eq: Eq[TelescopeOffset] =
      Eq.by(x => (x.p, x.q))

    implicit val show: Show[TelescopeOffset] = Show.fromToString
  }



  @Lenses final case class Conditions(
    cc: CloudCover,
    iq: ImageQuality,
    sb: SkyBackground,
    wv: WaterVapor
  )

  object Conditions {

    val worst: Conditions = Conditions(
      CloudCover.Any,
      ImageQuality.Any,
      SkyBackground.Any,
      WaterVapor.Any
    )

    val nominal: Conditions = Conditions(
      CloudCover.Percent50,
      ImageQuality.Percent70,
      SkyBackground.Percent50,
      WaterVapor.Any
    )

    val best: Conditions = Conditions(
      // In the ODB model it's 20% but that value it's marked as obsolete
      // so I took the non-obsolete lowest value.
      CloudCover.Percent50,
      ImageQuality.Percent20,
      SkyBackground.Percent20,
      WaterVapor.Percent20
    )

    val default: Conditions = worst // Taken from ODB

    implicit val equalConditions: Eq[Conditions] =
      Eq.by { x =>
        (x.cc, x.iq, x.sb, x.wv)
      }

    implicit val showConditions: Show[Conditions] = Show.show[Conditions] {
      case Conditions(cc, iq, sb, wv) => List(cc, iq, sb, wv).mkString(", ")
    }
  }




  // Log message types
  type Time = java.time.Instant

  trait LogType
  object LogType {
    object Debug
    object Info
    object Warning
    object Error
  }

  final case class LogMsg(t: LogType, timestamp: Time, msg: String)

}
//scalastyle:on
