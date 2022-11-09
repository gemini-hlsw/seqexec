// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.Monoid
import cats.syntax.all._
import lucuma.core.enums.GpiDisperser
import lucuma.core.enums.GpiFilter
import lucuma.core.enums.GpiObservingMode
import lucuma.core.math.Offset
import seqexec.model.NodAndShuffleStatus
import seqexec.model.NodAndShuffleStep
import seqexec.model.Observation
import seqexec.model.OffsetConfigResolver
import seqexec.model.SequenceState
import seqexec.model.Step
import seqexec.model.StepId
import seqexec.model.enum.Guiding
import seqexec.model.enum.Instrument
import seqexec.model.enum.StepType
import seqexec.model.enumerations
import seqexec.web.client.model.Formatting._
import seqexec.web.client.model.lenses._

/**
 * Contains methods to access details of a step normally stored on the step sequence
 */
object StepItems {

  private val gpiObsMode = GpiObservingMode.all.map(x => x.shortName -> x).toMap

  private val gpiFiltersMap: Map[String, GpiFilter] =
    GpiFilter.all.map(x => (x.shortName, x)).toMap

  val gpiDispersers: Map[String, String] =
    GpiDisperser.all.map(x => x.shortName -> x.longName).toMap

  private val obsNames =
    GpiObservingMode.all.map(x => x.shortName -> x.longName).toMap

  implicit class StepOps(val s: Step) extends AnyVal {
    def fpuNameMapper(i: Instrument): String => Option[String] =
      i match {
        case Instrument.GmosS => enumerations.fpu.GmosSFPU.get
        case Instrument.GmosN => enumerations.fpu.GmosNFPU.get
        case _                => _ => none
      }

    def exposureTime: Option[Double] = observeExposureTimeO.getOption(s)

    def exposureTimeS(i: Instrument): Option[String] =
      exposureTime.map(formatExposureTime(i))

    def exposureAndCoaddsS(i: Instrument): Option[String] =
      (coAdds, exposureTime) match {
        case (c, Some(e)) if c.exists(_ > 1) =>
          s"${c.foldMap(_.show)}x${formatExposureTime(i)(e)} s".some
        case (_, Some(e))                    =>
          s"${formatExposureTime(i)(e)} s".some
        case _                               => none
      }
    def coAdds: Option[Int]                               = observeCoaddsO.getOption(s)

    def fpu(i: Instrument): Option[String] =
      (i, instrumentFPUO.getOption(s), instrumentFPUCustomMaskO.getOption(s)) match {
        case (Instrument.GmosS | Instrument.GmosN | Instrument.F2, Some("CUSTOM_MASK"), c) => c
        case (Instrument.GmosS | Instrument.GmosN | Instrument.F2, None, c @ Some(_))      => c
        case (Instrument.F2, Some("Custom Mask"), c)                                       => c
        case (Instrument.F2, a @ Some(_), _)                                               => a
        case (_, Some(b), _)                                                               =>
          fpuNameMapper(i)(b)
        case _                                                                             => none
      }

    def fpuOrMask(i: Instrument): Option[String] =
      fpu(i)
        .orElse(instrumentSlitWidthO.getOption(s))
        .orElse(instrumentMaskO.getOption(s))

    def alignAndCalib(i: Instrument): Option[StepType.AlignAndCalib.type] =
      i match {
        case Instrument.Gpi if stepClassO.getOption(s).exists(_ === "acq") =>
          StepType.AlignAndCalib.some
        case _                                                             => none
      }

    def nodAndShuffle(i: Instrument): Option[StepType] =
      i match {
        case Instrument.GmosS | Instrument.GmosN
            if isNodAndShuffleO.getOption(s).exists(identity) =>
          stepTypeO.getOption(s) match {
            case Some(StepType.Dark) => StepType.NodAndShuffleDark.some
            case _                   => StepType.NodAndShuffle.some
          }
        case _ => none
      }

    def stepType(instrument: Instrument): Option[StepType] =
      alignAndCalib(instrument)
        .orElse(nodAndShuffle(instrument))
        .orElse(stepTypeO.getOption(s))

    private def gpiFilter: Step => Option[String] =
      s => {
        // Read the filter, if not found deduce it from the obs mode
        val f: Option[GpiFilter] =
          instrumentFilterO.getOption(s).flatMap(gpiFiltersMap.get).orElse {
            for {
              m <- instrumentObservingModeO.getOption(s)
              o <- gpiObsMode.get(m)
              f <- o.filter
            } yield f
          }
        f.map(_.longName)
      }

    def filter(i: Instrument): Option[String] =
      i match {
        case Instrument.GmosS =>
          instrumentFilterO
            .getOption(s)
            .flatMap(enumerations.filter.GmosSFilter.get)
        case Instrument.GmosN =>
          instrumentFilterO
            .getOption(s)
            .flatMap(enumerations.filter.GmosNFilter.get)
        case Instrument.F2    =>
          instrumentFilterO
            .getOption(s)
        case Instrument.Niri  =>
          instrumentFilterO
            .getOption(s)
            .flatMap(enumerations.filter.Niri.get)
        case Instrument.Gnirs =>
          instrumentFilterO
            .getOption(s)
            .map(_.sentenceCase)
        case Instrument.Nifs  =>
          instrumentFilterO
            .getOption(s)
            .map(_.sentenceCase)
        case Instrument.Gsaoi =>
          instrumentFilterO
            .getOption(s)
            .map(_.sentenceCase)
        case Instrument.Gpi   => gpiFilter(s)
        case _                => None
      }

    private def disperserNameMapper(i: Instrument): Map[String, String] =
      i match {
        case Instrument.GmosS => enumerations.disperser.GmosSDisperser
        case Instrument.GmosN => enumerations.disperser.GmosNDisperser
        case Instrument.Gpi   => gpiDispersers
        case _                => Map.empty
      }

    def disperser(i: Instrument): Option[String] = {
      val disperser         = for {
        disperser <- instrumentDisperserO.getOption(s)
      } yield disperserNameMapper(i).getOrElse(disperser, disperser)
      val centralWavelength = instrumentDisperserLambdaO.getOption(s)

      // Format
      (disperser, centralWavelength) match {
        case (Some(d), Some(w)) => f"$d @ $w%.0f nm".some
        case (Some(d), None)    => d.some
        case _                  => none
      }
    }

    def offset[T, A](implicit
      resolver: OffsetConfigResolver[T, A],
      m:        Monoid[Offset.Component[A]]
    ): Offset.Component[A] =
      offsetF[T, A].fold(s).orEmpty

    def guiding: Boolean         = telescopeGuidingWithT.exist(_ === Guiding.Guide)(s)
    def readMode: Option[String] = instrumentReadModeO.getOption(s)

    def observingMode: Option[String] =
      instrumentObservingModeO
        .getOption(s)
        .flatMap(obsNames.get)

    def cameraName(i: Instrument): Option[String] =
      i match {
        case Instrument.Niri =>
          instrumentCameraO
            .getOption(s)
            .flatMap(enumerations.camera.Niri.get)
        case _               => None
      }

    def deckerName: Option[String] =
      instrumentDeckerO.getOption(s)

    def imagingMirrorName: Option[String] =
      instrumentImagingMirrorO.getOption(s)
  }

  implicit class OffsetFnsOps(val steps: List[Step]) extends AnyVal {

    // Offsets to be displayed with a width
    def offsetsDisplay: OffsetsDisplay =
      (OffsetsDisplay.DisplayOffsets.apply _).tupled(steps.sequenceOffsetMaxWidth)

  }

  sealed abstract class DetailRows(val rows: Int)
  object DetailRows {
    final case object NoDetailRows  extends DetailRows(0)
    final case object OneDetailRow  extends DetailRows(1)
    final case object TwoDetailRows extends DetailRows(2)
  }

  final case class StepStateSummary(
    step:          Step,
    obsId:         Observation.Id,
    instrument:    Instrument,
    tabOperations: TabOperations,
    state:         SequenceState
  ) {
    val isAC: Boolean =
      step.alignAndCalib(instrument).isDefined

    val isNS: Boolean =
      step.nodAndShuffle(instrument).isDefined

    private val isRunning =
      tabOperations.resourceInFlight(step.id) || step.isRunning

    val isACRunning: Boolean =
      isAC && isRunning

    val isNSRunning: Boolean =
      isNS && isRunning

    val isNSObserving: Boolean =
      isNS && step.isRunning

    val anyError: Boolean =
      tabOperations.resourceInError(step.id) || step.hasError

    val isACInError: Boolean =
      isAC && anyError

    val isNSInError: Boolean =
      isNS && anyError

    val isBias: Boolean =
      step.stepType(instrument).exists(_ === StepType.Bias)

    def canControlThisStep(selected: Option[StepId], hasControls: Boolean): Boolean =
      hasControls && selected.exists(_ === step.id)

    def detailRows(selected: Option[StepId], hasControls: Boolean): DetailRows =
      if (((isNS || isNSInError) && canControlThisStep(selected, hasControls)) || isNSObserving)
        DetailRows.TwoDetailRows
      else if (isACRunning || isACInError)
        DetailRows.OneDetailRow
      else
        DetailRows.NoDetailRows

    val nsStatus: Option[NodAndShuffleStatus] = step match {
      case x: NodAndShuffleStep => Some(x.nsStatus)
      case _                    => None
    }

    val nsPendingObserveCmd: Option[NodAndShuffleStep.PendingObserveCmd] = step match {
      case x: NodAndShuffleStep => x.pendingObserveCmd
      case _                    => None
    }
  }

  object StepStateSummary {
    implicit val EqStepStateSummary: Eq[StepStateSummary] =
      Eq.by(x => (x.step, x.obsId, x.instrument, x.tabOperations, x.state))
  }

}
