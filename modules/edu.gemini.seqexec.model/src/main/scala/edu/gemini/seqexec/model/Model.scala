// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import scalaz._
import Scalaz._

import monocle.{Lens, Prism}
import monocle.macros.GenLens
import monocle.Traversal

import java.time.Instant

object Model {
  // We use this to avoid a dependency on spModel, should be replaced by gem
  sealed trait SeqexecSite {
    def instruments: NonEmptyList[Instrument]
  }
  object SeqexecSite {
    case object SeqexecGN extends SeqexecSite {
      val instruments = Instrument.gnInstruments
    }

    case object SeqexecGS extends SeqexecSite {
      val instruments = Instrument.gsInstruments
    }

    implicit val show: Show[SeqexecSite] = Show.shows({
      case SeqexecGN => "GN"
      case SeqexecGS => "GS"
    })
  }

  sealed trait ServerLogLevel
  object ServerLogLevel {
    case object INFO extends ServerLogLevel
    case object WARN extends ServerLogLevel
    case object ERROR extends ServerLogLevel
  }

  sealed trait SeqexecEvent
  sealed trait SeqexecModelUpdate extends SeqexecEvent {
    def view: SequencesQueue[SequenceView]
  }
  object SeqexecEvent {
    case class ConnectionOpenEvent(u: Option[UserDetails]) extends SeqexecEvent

    case class SequenceStart(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class StepExecuted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceCompleted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceLoaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceUnloaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class StepBreakpointChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class OperatorUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class ObserverUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class ConditionsUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class StepSkipMarkChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequencePauseRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class SequenceRefreshed(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    case class ResourcesBusy(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    // Generic update. It will probably become useless if we have a special Event for every case.
    case class SequenceUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    // TODO: msg should be LogMsg but it does IO when getting a timestamp, it
    // has to be embedded in a `Task`
    case class NewLogMessage(msg: String) extends SeqexecEvent
    case class ServerLogMessage(level: ServerLogLevel, timestamp: Instant, msg: String) extends SeqexecEvent
    case object NullEvent extends SeqexecEvent

    // Some useful Monocle lenses
    val obsNameL = GenLens[SequenceView](_.metadata.name)
    val eachL = Traversal.fromTraverse[List, SequenceView]
    val sequencesQueueL = GenLens[SequencesQueue[SequenceView]](_.queue)
    val ssLens = GenLens[SequenceStart](_.view)

    // Prism to focus on only the SeqexecEvents that have a queue
    // Unfortunately it doesn't seem to exist a more generic form to build this one
    val sePrism = Prism.partial[SeqexecEvent, (SeqexecEvent, SequencesQueue[SequenceView])]{
      case e @ StepExecuted(v)           => (e, v)
      case e @ SequenceStart(v)          => (e, v)
      case e @ SequenceCompleted(v)      => (e, v)
      case e @ SequenceLoaded(_, v)      => (e, v)
      case e @ SequenceUnloaded(_, v)    => (e, v)
      case e @ StepBreakpointChanged(v)  => (e, v)
      case e @ OperatorUpdated(v)        => (e, v)
      case e @ ObserverUpdated(v)        => (e, v)
      case e @ ConditionsUpdated(v)      => (e, v)
      case e @ StepSkipMarkChanged(v)    => (e, v)
      case e @ SequencePauseRequested(v) => (e, v)
      case e @ SequenceRefreshed(v)      => (e, v)
      case e @ ResourcesBusy(v)          => (e, v)
      case e @ SequenceUpdated(v)        => (e, v)
    } { case (e, q) =>
      e match {
        case SequenceStart(_)           => SequenceStart(q)
        case StepExecuted(_)            => StepExecuted(q)
        case SequenceCompleted(_)       => SequenceCompleted(q)
        case e @ SequenceLoaded(_, v)   => e.copy(view = q)
        case e @ SequenceUnloaded(_, v) => e.copy(view = q)
        case StepBreakpointChanged(_)   => StepBreakpointChanged(q)
        case OperatorUpdated(_)         => OperatorUpdated(q)
        case ObserverUpdated(_)         => ObserverUpdated(q)
        case ConditionsUpdated(_)       => ConditionsUpdated(q)
        case StepSkipMarkChanged(_)     => StepSkipMarkChanged(q)
        case SequencePauseRequested(_)  => SequencePauseRequested(q)
        case SequenceRefreshed(_)       => SequenceRefreshed(q)
        case ResourcesBusy(_)           => ResourcesBusy(q)
        case SequenceUpdated(_)         => SequenceUpdated(q)
        case e                          => e
      }
    }
    val tupleLens = Lens[(SeqexecEvent, SequencesQueue[SequenceView]), SequencesQueue[SequenceView]](_._2)(v => t => t.copy(_2 = v))
    // Composite lens to change the sequence name of an event
    val sequenceNameL = sePrism composeLens tupleLens composeLens sequencesQueueL composeTraversal eachL composeLens obsNameL
  }

  type SystemName = String
  type ParamName = String
  type ParamValue = String
  type Parameters = Map[ParamName, ParamValue]
  type StepConfig = Map[SystemName, Parameters]
  // TODO This should be a richer type
  type SequenceId = String
  type StepId = Int
  /**
    * A Seqexec resource represents any system that can be only used by one single agent.
    *
    */
  sealed trait Resource

  object Resource {

    case object P1 extends Resource
    case object OI extends Resource
    // Mount and science fold cannot be controlled independently. Maybe in the future.
    // For now, I replaced them with TCS
  //  case object Mount extends Resource
  //  case object ScienceFold extends Resource
    case object TCS extends Resource
    case object Gcal extends Resource
    case object Gems extends Resource
    case object Altair extends Resource

  }
  sealed trait Instrument extends Resource
  object Instrument {
    case object F2 extends Instrument
    case object GmosS extends Instrument
    case object GmosN extends Instrument
    case object GNIRS extends Instrument
    case object GPI extends Instrument
    case object GSAOI extends Instrument
    case object NIRI extends Instrument
    case object NIFS extends Instrument

    implicit val equal: Equal[Instrument] = Equal.equalA[Instrument]
    implicit val show: Show[Instrument] = Show.shows({
      case F2    => "Flamingos2"
      case GmosS => "GMOS-S"
      case GmosN => "GMOS-N"
      case GPI   => "GPI"
      case GSAOI => "GSAOI"
      case GNIRS => "GNIRS"
      case NIRI  => "NIRI"
      case NIFS  => "NIFS"
    })
    val gsInstruments = NonEmptyList[Instrument](F2, GmosS, GPI, GSAOI)
    val gnInstruments = NonEmptyList[Instrument](GmosN, GNIRS, NIRI, NIFS)
  }

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

    def isError(state: SequenceState): Boolean = state match {
      case Error(_) => true
      case _        => false
    }

    implicit val equal: Equal[SequenceState] = Equal.equalA[SequenceState]
  }

  /**
    * Metadata about the sequence required on the exit point
    */
  // TODO Une a proper instrument class
  case class SequenceMetadata(
    instrument: Instrument,
    observer: Option[Observer],
    name: String
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
  case class SequencesQueue[T](conditions: Conditions, operator: Option[Operator], queue: List[T])

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

  sealed trait CloudCover {
    val toInt: Int
  }
  object CloudCover {
    case object Percent50 extends CloudCover { override val toInt: Int = 50  }
    case object Percent70 extends CloudCover { override val toInt: Int = 70  }
    case object Percent80 extends CloudCover { override val toInt: Int = 80  }
    case object Any       extends CloudCover { override val toInt: Int = 100 } // ODB is 100

    val all: List[CloudCover] = List(Percent50, Percent70, Percent80, Any)

    implicit val equalCloudCover: Equal[CloudCover] = Equal.equalA[CloudCover]

    implicit val showCloudCover: Show[CloudCover] = Show.shows[CloudCover] {
      case Percent50 => "50%/Clear"
      case Percent70 => "70%/Cirrus"
      case Percent80 => "80%/Cloudy"
      case Any       => "Any"
    }

  }

  sealed trait ImageQuality {
    val toInt: Int
  }
  object ImageQuality {
    case object Percent20 extends ImageQuality { override val toInt: Int = 20  }
    case object Percent70 extends ImageQuality { override val toInt: Int = 70  }
    case object Percent85 extends ImageQuality { override val toInt: Int = 85  }
    case object Any       extends ImageQuality { override val toInt: Int = 100 } // ODB is 100

    val all: List[ImageQuality] = List(Percent20, Percent70, Percent85, Any)

    implicit val equalImageQuality: Equal[ImageQuality] = Equal.equalA[ImageQuality]

    implicit val showImageQuality: Show[ImageQuality] = Show.shows[ImageQuality] {
      case Percent20 => "20%/Best"
      case Percent70 => "70%/Good"
      case Percent85 => "85%/Poor"
      case Any       => "Any"
    }

  }

  sealed trait SkyBackground {
    val toInt: Int
  }
  object SkyBackground {
    case object Percent20 extends SkyBackground { override val toInt: Int = 20  }
    case object Percent50 extends SkyBackground { override val toInt: Int = 50  }
    case object Percent80 extends SkyBackground { override val toInt: Int = 80  }
    case object Any       extends SkyBackground { override val toInt: Int = 100 } // ODB is 100

    val all: List[SkyBackground] = List(Percent20, Percent50, Percent80, Any)

    implicit val equal: Equal[SkyBackground] = Equal.equalA[SkyBackground]

    implicit val showSkyBackground: Show[SkyBackground] = Show.shows[SkyBackground] {
      case Percent20 => "20%/Darkest"
      case Percent50 => "50%/Dark"
      case Percent80 => "80%/Grey"
      case Any       => "Any/Bright"
    }

  }

  sealed trait WaterVapor {
    val toInt: Int
  }
  object WaterVapor {
    case object Percent20 extends WaterVapor { override val toInt: Int = 20  }
    case object Percent50 extends WaterVapor { override val toInt: Int = 50  }
    case object Percent80 extends WaterVapor { override val toInt: Int = 80  }
    case object Any       extends WaterVapor { override val toInt: Int = 100 } // ODB is 100

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
