// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import scalaz._
import Scalaz._

import monocle.{Lens, Prism, PTraversal}
import monocle.macros.{GenLens, GenPrism, Lenses}
import monocle.Traversal

import java.time.Instant

import dhs.ImageFileId

@SuppressWarnings(Array("org.wartremover.warts.PublicInference", "org.wartremover.warts.IsInstanceOf"))
object Model {
  // We use this to avoid a dependency on spModel, should be replaced by gem
  sealed trait SeqexecSite {
    def instruments: NonEmptyList[Instrument]
  }
  object SeqexecSite {
    case object SeqexecGN extends SeqexecSite {
      val instruments: NonEmptyList[Instrument] = Instrument.gnInstruments
    }

    case object SeqexecGS extends SeqexecSite {
      val instruments : NonEmptyList[Instrument]= Instrument.gsInstruments
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

  object SeqexecModelUpdate {
    implicit val equal: Equal[SeqexecModelUpdate] = Equal.equalA
  }
  object SeqexecEvent {
    final case class ConnectionOpenEvent(u: Option[UserDetails]) extends SeqexecEvent

    final case class SequenceStart(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    object SequenceStart {
      implicit val equal: Equal[SequenceStart] = Equal.equalA
    }

    final case class StepExecuted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class FileIdStepExecuted(fileId: ImageFileId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceCompleted(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceLoaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceUnloaded(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class StepBreakpointChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class OperatorUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ObserverUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ConditionsUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class StepSkipMarkChanged(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequencePauseRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequencePauseCanceled(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceRefreshed(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ActionStopRequested(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class ResourcesBusy(obsId: SequenceId, view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    final case class SequenceUpdated(view: SequencesQueue[SequenceView]) extends SeqexecModelUpdate

    // TODO: msg should be LogMsg but it does IO when getting a timestamp, it
    // has to be embedded in a `Task`
    final case class NewLogMessage(msg: String) extends SeqexecEvent
    final case class ServerLogMessage(level: ServerLogLevel, timestamp: Instant, msg: String) extends SeqexecEvent
    case object NullEvent extends SeqexecEvent

    // Some useful Monocle lenses
    val obsNameL: Lens[SequenceView, String] = GenLens[SequenceView](_.metadata.name)
    val eachL: Traversal[List[SequenceView], SequenceView] = Traversal.fromTraverse[List, SequenceView]
    val sequencesQueueL: Lens[SequencesQueue[SequenceView], List[SequenceView]] = GenLens[SequencesQueue[SequenceView]](_.queue)
    val ssLens: Lens[SequenceStart, SequencesQueue[SequenceView]] = GenLens[SequenceStart](_.view)

    // Prism to focus on only the SeqexecEvents that have a queue
    val sePrism: Prism[SeqexecEvent, SeqexecModelUpdate] = GenPrism[SeqexecEvent, SeqexecModelUpdate]

    val seViewL: Lens[SeqexecModelUpdate, SequencesQueue[SequenceView]] = Lens[SeqexecModelUpdate, SequencesQueue[SequenceView]](_.view)(q => {
        case e @ SequenceStart(_)           => e.copy(view = q)
        case e @ StepExecuted(_)            => e.copy(view = q)
        case e @ FileIdStepExecuted(_, _)   => e.copy(view = q)
        case e @ SequenceCompleted(_)       => e.copy(view = q)
        case e @ SequenceLoaded(_, v)       => e.copy(view = q)
        case e @ SequenceUnloaded(_, v)     => e.copy(view = q)
        case e @ StepBreakpointChanged(_)   => e.copy(view = q)
        case e @ OperatorUpdated(_)         => e.copy(view = q)
        case e @ ObserverUpdated(_)         => e.copy(view = q)
        case e @ ConditionsUpdated(_)       => e.copy(view = q)
        case e @ StepSkipMarkChanged(_)     => e.copy(view = q)
        case e @ SequencePauseRequested(_)  => e.copy(view = q)
        case e @ SequencePauseCanceled(_)   => e.copy(view = q)
        case e @ SequenceRefreshed(_)       => e.copy(view = q)
        case e @ ActionStopRequested(_)     => e.copy(view = q)
        case e @ ResourcesBusy(_, _)        => e.copy(view = q)
        case e @ SequenceUpdated(_)         => e.copy(view = q)
        case e                              => e
      }
    )
    // Composite lens to change the sequence name of an event
    val sequenceNameL: PTraversal[SeqexecEvent, SeqexecEvent, String, String] = sePrism composeLens seViewL composeLens sequencesQueueL composeTraversal eachL composeLens obsNameL

    implicit val equal: Equal[SeqexecEvent] = Equal.equalA
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

    implicit val order: Order[Resource] = Order.orderBy {
      case TCS               => 1
      case Gcal              => 2
      case Gems              => 3
      case Altair            => 4
      case OI                => 5
      case P1                => 6
      case Instrument.F2     => 11
      case Instrument.GmosS  => 12
      case Instrument.GmosN  => 13
      case Instrument.GPI    => 14
      case Instrument.GSAOI  => 15
      case Instrument.GNIRS  => 16
      case Instrument.NIRI   => 17
      case Instrument.NIFS   => 18
    }
    implicit val ordering: scala.math.Ordering[Resource] = order.toScalaOrdering
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
    val gsInstruments: NonEmptyList[Instrument] = NonEmptyList[Instrument](F2, GmosS, GPI, GSAOI)
    val gnInstruments: NonEmptyList[Instrument] = NonEmptyList[Instrument](GmosN, GNIRS, NIRI, NIFS)
  }

  final case class Operator(value: String)

  object Operator {
    val Zero: Operator = Operator("")
    implicit val equal: Equal[Operator] = Equal.equalA
    implicit val shows: Show[Operator] = Show.shows(_.value)
  }

  final case class Observer(value: String)
  object Observer {
    val Zero: Observer = Observer("")
    implicit val equal: Equal[Observer] = Equal.equalA
    implicit val shows: Show[Observer] = Show.shows(_.value)
  }

  implicit val equalSequenceId: Equal[SequenceId] = Equal.equalA[SequenceId]

  sealed trait StepState {
    def canRunFrom: Boolean = false
  }
  object StepState {
    case object Pending extends StepState {
      override val canRunFrom: Boolean = true
    }
    case object Completed extends StepState
    case object Skipped extends StepState
    final case class Error(msg: String) extends StepState {
      override val canRunFrom: Boolean = true
    }
    case object Running extends StepState
    case object Paused extends StepState {
      override val canRunFrom: Boolean = true
    }

    implicit val equal: Equal[StepState] = Equal.equalA[StepState]
  }

  sealed trait ActionStatus
  object ActionStatus {
    case object Pending extends ActionStatus
    case object Completed extends ActionStatus
    case object Running extends ActionStatus

    implicit val equal: Equal[ActionStatus] = Equal.equalA[ActionStatus]
  }

  sealed trait Step {
    val id: StepId
    val config: StepConfig
    val status: StepState
    val breakpoint: Boolean
    val skip: Boolean
    val fileId: Option[dhs.ImageFileId]
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
  // Other kinds of Steps to be defined.

  sealed trait SequenceState
  object SequenceState {
    case object Completed         extends SequenceState
    case object Running           extends SequenceState
    case object Pausing           extends SequenceState
    case object Stopping          extends SequenceState
    case object Idle              extends SequenceState
    case object Paused            extends SequenceState
    final case class Error(msg: String) extends SequenceState

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
  @Lenses final case class SequenceMetadata(
    instrument: Instrument,
    observer: Option[Observer],
    name: String
  )

  @Lenses final case class SequenceView (
    id: SequenceId,
    metadata: SequenceMetadata,
    status: SequenceState,
    steps: List[Step],
    willStopIn: Option[Int]
  )

  object SequenceView {
    implicit val eq: Equal[SequenceView] = Equal.equalA
  }

  /**
    * Represents a queue with different levels of details. E.g. it could be a list of Ids
    * Or a list of fully hydrated SequenceViews
    */
  final case class SequencesQueue[T](conditions: Conditions, operator: Option[Operator], queue: List[T])

  object SequencesQueue {
    implicit def equal[T: Equal]: Equal[SequencesQueue[T]] = Equal.equalA
  }

  // Ported from OCS' SPSiteQuality.java

  final case class Conditions(
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

  final case class LogMsg(t: LogType, timestamp: Time, msg: String)

}
