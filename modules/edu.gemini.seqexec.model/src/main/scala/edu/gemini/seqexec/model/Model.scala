// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import monocle.macros.Lenses

import scalaz.{Equal, NonEmptyList, Order, Show}
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.syntax.show._
import scalaz.syntax.equal._
import scalaz.syntax.std.option._
import java.time.ZoneId

@SuppressWarnings(Array("org.wartremover.warts.PublicInference", "org.wartremover.warts.IsInstanceOf"))
// scalastyle:off
object Model {
  // We use this to avoid a dependency on spModel, should be replaced by gem
  sealed trait SeqexecSite {
    def instruments: NonEmptyList[Instrument]
    def timeZone: ZoneId
  }
  object SeqexecSite {
    final case class SeqexecGN(timeZone: ZoneId) extends SeqexecSite {
      val instruments: NonEmptyList[Instrument] = Instrument.gnInstruments
    }

    final case class SeqexecGS(timeZone: ZoneId) extends SeqexecSite {
      val instruments : NonEmptyList[Instrument]= Instrument.gsInstruments
    }

    implicit val show: Show[SeqexecSite] = Show.shows({
      case SeqexecGN(_) => "GN"
      case SeqexecGS(_) => "GS"
    })
  }

  sealed trait ServerLogLevel
  object ServerLogLevel {
    case object INFO extends ServerLogLevel
    case object WARN extends ServerLogLevel
    case object ERROR extends ServerLogLevel

    val all: List[ServerLogLevel] = List(INFO, WARN, ERROR)

    implicit val show: Show[ServerLogLevel] = Show.shows {
      case INFO  => "INFO"
      case WARN  => "WARNING"
      case ERROR => "ERROR"
    }
  }

  // The system name in ocs is a string but we can represent the important ones as an ADT
  sealed trait SystemName {
    def withParam(p: String): String = s"${this.shows}:$p"
  }
  object SystemName {
    case object ocs extends SystemName
    case object observe extends SystemName
    case object instrument extends SystemName
    case object telescope extends SystemName
    case object gcal extends SystemName
    case object calibration extends SystemName
    case object meta extends SystemName

    def unsafeFromString(system: String): SystemName = system match {
      case "ocs"         => ocs
      case "instrument"  => instrument
      case "telescope"   => telescope
      case "gcal"        => gcal
      case "observe"     => observe
      case "calibration" => calibration
      case "meta"        => meta
      case s             => sys.error(s"Unknown system name $s")
    }

    val all: List[SystemName] = List(ocs, instrument, telescope, gcal)

    implicit val show: Show[SystemName] = Show.shows {
      case `ocs`         => "ocs"
      case `instrument`  => "instrument"
      case `telescope`   => "telescope"
      case `gcal`        => "gcal"
      case `observe`     => "observe"
      case `calibration` => "calibration"
      case `meta`        => "meta"
    }
    implicit val equal: Equal[SystemName] = Equal.equalA[SystemName]
  }
  type ParamName = String
  type ParamValue = String
  type Parameters = Map[ParamName, ParamValue]
  type StepConfig = Map[SystemName, Parameters]
  implicit val stEqual: Equal[StepConfig] = Equal.equalA[StepConfig]
  // TODO This should be a richer type
  type SequenceId = String
  type StepId = Int
  type ObservationName = String
  type TargetName = String
  val DaytimeCalibrationTargetName = "Daytime calibration"

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
    implicit val show: Show[Resource] = Show.shows {
      case TCS               => "TCS"
      case Gcal              => "Gcal"
      case Gems              => "Gems"
      case Altair            => "Altair"
      case OI                => "OI"
      case P1                => "P1"
      case i: Instrument     => i.shows
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
    implicit val show: Show[Instrument] = Show.shows {
      case F2    => "Flamingos2"
      case GmosS => "GMOS-S"
      case GmosN => "GMOS-N"
      case GPI   => "GPI"
      case GSAOI => "GSAOI"
      case GNIRS => "GNIRS"
      case NIRI  => "NIRI"
      case NIFS  => "NIFS"
    }
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
    final case class Failed(msg: String) extends StepState {
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
    // Action is not yet run
    case object Pending extends ActionStatus
    // Action run and completed
    case object Completed extends ActionStatus
    // Action currently running
    case object Running extends ActionStatus
    // Action run but paused
    case object Paused extends ActionStatus
    // Action run but failed to complete
    case object Failed extends ActionStatus

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
  object Step {
    val Zero: Step = StandardStep(id = -1, config = Map.empty, status = StepState.Pending, breakpoint = false, skip = false, fileId = None, configStatus = Nil, observeStatus = ActionStatus.Pending)

    implicit val equal: Equal[Step] = Equal.equalA[Step]

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
    implicit val equal: Equal[StandardStep] = Equal.equalA[StandardStep]
  }
  // Other kinds of Steps to be defined.

  sealed trait SequenceState
  object SequenceState {
    case object Completed         extends SequenceState
    case object Idle              extends SequenceState
    case object Stopped           extends SequenceState
    final case class Running(userStop: Boolean, internalStop: Boolean) extends SequenceState
    object Running {
      val init: Running = Running(false, false)
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

      def isInProcess: Boolean = state =/= SequenceState.Idle

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

  // Complements to the science model
  sealed trait StepType
  object StepType {
    case object Object extends StepType
    case object Arc extends StepType
    case object Flat extends StepType
    case object Bias extends StepType
    case object Dark extends StepType
    case object Calibration extends StepType

    implicit val eq: Equal[StepType] = Equal.equalA[StepType]
    implicit val show: Show[StepType] = Show.shows {
      case Object      => "OBJECT"
      case Arc         => "ARC"
      case Flat        => "FLAT"
      case Bias        => "BIAS"
      case Dark        => "DARK"
      case Calibration => "CAL"
    }

    val all: List[StepType] = List(Object, Arc, Flat, Bias, Dark, Calibration)
    private val names = all.map(x => (x.shows, x)).toMap

    def fromString(s: String): Option[StepType] = names.get(s)
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
    implicit val show: Show[OffsetAxis] = Show.shows {
      case AxisP => "p"
      case AxisQ => "q"
    }
  }

  sealed trait Offset {
    val value: Double
  }
  object Offset {
    implicit val equal: Equal[Offset] = Equal.equalA
    def Zero(axis: OffsetAxis): Offset = axis match {
      case OffsetAxis.AxisP => TelescopeOffset.P.Zero
      case OffsetAxis.AxisQ => TelescopeOffset.Q.Zero
    }
  }

  sealed trait Guiding {
    val configValue: String
  }
  object Guiding {
    case object Guide extends Guiding {
      val configValue: String = "guide"
    }
    case object Park extends Guiding {
      val configValue: String = "park"
    }
    case object Freeze extends Guiding {
      val configValue: String = "freeze"
    }

    implicit val equal: Equal[Guiding] = Equal.equalA

    def fromString(s: String): Option[Guiding] = s match {
      case "guide"  => Guiding.Guide.some
      case "park"   => Guiding.Park.some
      case "freeze" => Guiding.Freeze.some
      case _        => none
    }
  }
  sealed trait FPUMode
  object FPUMode {
    case object BuiltIn extends FPUMode
    case object Custom extends FPUMode

    implicit val equal: Equal[FPUMode] = Equal.equalA
    implicit val show: Show[FPUMode] = Show.showFromToString

    def fromString(s: String): Option[FPUMode] = s match {
      case "BUILTIN"     => FPUMode.BuiltIn.some
      case "CUSTOM_MASK" => FPUMode.Custom.some
      case _             => none
    }
  }

  // Telescope offsets, roughly based on gem
  final case class TelescopeOffset(p: TelescopeOffset.P, q: TelescopeOffset.Q)
  object TelescopeOffset {
    /** P component of an angular offset.. */
    final case class P(value: Double) extends Offset
    object P {
      val Zero: P = P(0.0)
      implicit val order: Order[P] = Order.orderBy(_.value)

    }
    /** Q component of an angular offset.. */
    final case class Q(value: Double) extends Offset
    object Q {
      val Zero: Q = Q(0.0)
      implicit val order: Order[Q] = Order.orderBy(_.value)

    }
    implicit val eq: Equal[TelescopeOffset] = Equal.equalA[TelescopeOffset]
    implicit val show: Show[TelescopeOffset] = Show.showFromToString
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
//scalastyle:on
