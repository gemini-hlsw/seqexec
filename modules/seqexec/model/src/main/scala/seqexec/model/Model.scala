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





  // // Log message types
  // type Time = java.time.Instant

  // trait LogType
  // object LogType {
  //   object Debug
  //   object Info
  //   object Warning
  //   object Error
  // }

  // final case class LogMsg(t: LogType, timestamp: Time, msg: String)

}
//scalastyle:on
