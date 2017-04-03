package edu.gemini.seqexec.model

import scalaz._
import Scalaz._

object Model {

  sealed trait SeqexecEvent
  sealed trait SeqexecModelUpdate extends SeqexecEvent {
    def view: SequencesQueue[SequenceView]
  }
  object SeqexecEvent {
    case class ConnectionOpenEvent(u: Option[UserDetails]) extends SeqexecEvent

    case class SequenceStart(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class StepExecuted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceCompleted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceLoaded(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class StepBreakpointChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class OperatorUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class ObserverUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class ConditionsUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class StepSkipMarkChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequencePauseRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceRefreshed(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    //Generic update. It will probably become useless if we have a special Event for every case.
    case class SequenceUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    // TODO: msg should be LogMsg bit it does IO when getting a timestamp, it
    // has to be embedded in a `Task`
    case class NewLogMessage(msg: String) extends SeqexecEvent

  }

  type SystemName = String
  type ParamName = String
  type ParamValue = String
  type Parameters = Map[ParamName, ParamValue]
  type StepConfig = Map[SystemName, Parameters]
  // TODO This should be a richer type
  type SequenceId = String
  type StepId = Int
  type Instrument = String
  type Operator = String
  type Observer = String

  implicit val equalSequenceId: Equal[SequenceId] = Equal.equalA[SequenceId]

  sealed trait StepState {
    def canRunFrom: Boolean = false
  }
  object StepState {
    case object Pending extends StepState {
      override val canRunFrom = true
    }
    case object Completed extends StepState
    case object Skipped extends StepState
    case class Error(msg: String) extends StepState {
      override val canRunFrom = true
    }
    case object Running extends StepState
    case object Paused extends StepState {
      override val canRunFrom = true
    }

    implicit val equal: Equal[StepState] = Equal.equalA[StepState]
  }

  sealed trait ActionStatus
  object ActionStatus {
    case object Pending extends ActionStatus
    case object Completed extends ActionStatus
    case class Running(progress: Double) extends ActionStatus
  }

  sealed trait Step {
    val id: StepId
    val config: StepConfig
    val status: StepState
    val breakpoint: Boolean
    val skip: Boolean
    val fileId: Option[dhs.ImageFileId]
  }

  case class StandardStep(
    override val id: StepId,
    override val config: StepConfig,
    override val status: StepState,
    override val breakpoint: Boolean,
    override val skip: Boolean,
    override val fileId: Option[dhs.ImageFileId],
    configStatus: Map[SystemName, ActionStatus],
    observeStatus: ActionStatus
  ) extends Step
  // Other kinds of Steps to be defined.

  sealed trait SequenceState
  object SequenceState {
    case object Completed         extends SequenceState
    case object Running           extends SequenceState
    case object Stopping          extends SequenceState
    case object Idle              extends SequenceState
    case object Paused            extends SequenceState
    case class Error(msg: String) extends SequenceState

    implicit val equal: Equal[SequenceState] = Equal.equalA[SequenceState]
  }

  /**
    * Metadata about the sequence required on the exit point
    */
  // TODO Une a proper instrument class
  case class SequenceMetadata(
    instrument: Instrument,
    operator: Option[Operator],
    observer: Option[Observer]
  )

  case class SequenceView (
    id: SequenceId,
    metadata: SequenceMetadata,
    status: SequenceState,
    steps: List[Step],
    willStopIn: Option[Int]
  )

  /**
    * Represents a queue with different levels of details. E.g. it could be a list of Ids
    * Or a list of fully hydrated SequenceViews
    */
  case class SequencesQueue[T](conditions: Conditions, queue: List[T])

  // Ported from OCS' SPSiteQuality.java

  final case class Conditions(
    cc: CloudCover,
    iq: ImageQuality,
    sb: SkyBackground,
    wv: WaterVapor
  )

  object Conditions {

    val worst = Conditions(
      CloudCover.Any,
      ImageQuality.Any,
      SkyBackground.Any,
      WaterVapor.Any
    )

    val nominal = Conditions(
      CloudCover.Percent50,
      ImageQuality.Percent70,
      SkyBackground.Percent50,
      WaterVapor.Any
    )

    val best = Conditions(
      // In the ODB model it's 20% but that value it's marked as obsolete
      // so I took the non-obsolete lowest value.
      CloudCover.Percent50,
      ImageQuality.Percent20,
      SkyBackground.Percent20,
      WaterVapor.Percent20
    )

    val default = worst // Taken from ODB

    implicit val equalConditions: Equal[Conditions] = Equal.equalA[Conditions]

    implicit val showConditions: Show[Conditions] = Show.shows[Conditions] {
      case Conditions(cc, iq, sb, wv) => List(cc, iq, sb, wv).mkString(", ")
    }

  }

  sealed trait CloudCover
  object CloudCover {
    case object Percent50 extends CloudCover { val toInt: Int = 50  }
    case object Percent70 extends CloudCover { val toInt: Int = 70  }
    case object Percent80 extends CloudCover { val toInt: Int = 80  }
    case object Any       extends CloudCover { val toInt: Int = 100 } // ODB is 100

    val all: List[CloudCover] = List(Percent50, Percent70, Percent80, Any)

    implicit val equalCloudCover: Equal[CloudCover] = Equal.equalA[CloudCover]

    implicit val showCloudCover: Show[CloudCover] = Show.shows[CloudCover] {
      case Percent50 => "50%/Clear"
      case Percent70 => "70%/Cirrus"
      case Percent80 => "80%/Cloudy"
      case Any       => "Any"
    }

  }

  sealed trait ImageQuality
  object ImageQuality {
    case object Percent20 extends ImageQuality { val toInt: Int = 20  }
    case object Percent70 extends ImageQuality { val toInt: Int = 70  }
    case object Percent85 extends ImageQuality { val toInt: Int = 85  }
    case object Any       extends ImageQuality { val toInt: Int = 100 } // ODB is 100

    val all: List[ImageQuality] = List(Percent20, Percent70, Percent85, Any)

    implicit val equalImageQuality: Equal[ImageQuality] = Equal.equalA[ImageQuality]

    implicit val showImageQuality: Show[ImageQuality] = Show.shows[ImageQuality] {
      case Percent20 => "20%/Best"
      case Percent70 => "70%/Good"
      case Percent85 => "85%/Poor"
      case Any       => "Any"
    }

  }

  sealed trait SkyBackground
  object SkyBackground {
    case object Percent20 extends SkyBackground { val toInt: Int = 20  }
    case object Percent50 extends SkyBackground { val toInt: Int = 50  }
    case object Percent80 extends SkyBackground { val toInt: Int = 80  }
    case object Any       extends SkyBackground { val toInt: Int = 100 } // ODB is 100

    val all: List[SkyBackground] = List(Percent20, Percent50, Percent80, Any)

    implicit val equal: Equal[SkyBackground] = Equal.equalA[SkyBackground]

    implicit val showSkyBackground: Show[SkyBackground] = Show.shows[SkyBackground] {
      case Percent20 => "20%/Darkest"
      case Percent50 => "50%/Dark"
      case Percent80 => "80%/Grey"
      case Any       => "Any/Bright"
    }

  }

  sealed trait WaterVapor
  object WaterVapor {
    case object Percent20 extends WaterVapor { val toInt: Int = 20  }
    case object Percent50 extends WaterVapor { val toInt: Int = 50  }
    case object Percent80 extends WaterVapor { val toInt: Int = 80  }
    case object Any       extends WaterVapor { val toInt: Int = 100 } // ODB is 100

    val all: List[WaterVapor] = List(Percent20, Percent50, Percent80, Any)

    implicit val equal: Equal[WaterVapor] = Equal.equalA[WaterVapor]

    implicit val showWaterVapor: Show[WaterVapor] = Show.shows[WaterVapor] {
      case Percent20 => "20%/Low"
      case Percent50 => "50%/Median"
      case Percent80 => "85%/High"
      case Any       => "Any"
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

  case class LogMsg(t: LogType, timestamp: Time, msg: String)

  // Operations possible at the sequence level
  sealed trait SequenceOperations

  object SequenceOperations {
    case object PauseSequence extends SequenceOperations
    case object ContinueSequence extends SequenceOperations
    case object StopSequence extends SequenceOperations
    case object RunSequence extends SequenceOperations
  }

  // Operations possible at the observation level
  sealed trait ObservationOperations

  object ObservationOperations {
    case object PauseObservation extends ObservationOperations
    case object StopObservation extends ObservationOperations
    case object AbortObservation extends ObservationOperations
    case object ResumeObservation extends ObservationOperations
  }

  // Operations for Hamamatsu
  case object PauseImmediatelyObservation extends ObservationOperations
  case object StopImmediatelyObservation extends ObservationOperations
  case object PauseGracefullyObservation extends ObservationOperations
  case object StopGracefullyObservation extends ObservationOperations

}
