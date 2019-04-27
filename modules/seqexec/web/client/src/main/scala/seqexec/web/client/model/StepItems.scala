// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.implicits._
import gem.enum.GpiDisperser
import gem.enum.GpiObservingMode
import gem.enum.GpiFilter
import seqexec.model.enum.Instrument
import seqexec.model.enum.FPUMode
import seqexec.model.enum.Guiding
import seqexec.model.Step
import seqexec.model.StepState
import seqexec.model.enumerations
import seqexec.model.OffsetAxis
import seqexec.model.TelescopeOffset
import seqexec.web.client.model.lenses._
import seqexec.web.client.model.Formatting._

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
    def canRunFrom: Boolean = s.status match {
      case StepState.Pending | StepState.Failed(_) => true
      case _                                       => false
    }

    def fpuNameMapper(i: Instrument): String => Option[String] = i match {
      case Instrument.GmosS => enumerations.fpu.GmosSFPU.get
      case Instrument.GmosN => enumerations.fpu.GmosNFPU.get
      case Instrument.F2    => enumerations.fpu.Flamingos2.get
      case _ =>
        _ =>
          none
    }

    def exposureTime: Option[Double] = observeExposureTimeO.getOption(s)
    def exposureTimeS(i: Instrument): Option[String] =
      exposureTime.map(formatExposureTime(i))
    def exposureAndCoaddsS(i: Instrument): Option[String] =
      (coAdds, exposureTime) match {
        case (c, Some(e)) if c.exists(_ > 1) =>
          s"${c.foldMap(_.show)}x${formatExposureTime(i)(e)} [s]".some
        case (_, Some(e)) =>
          s"${formatExposureTime(i)(e)} [s]".some
        case _ => none
      }
    def coAdds: Option[Int] = observeCoaddsO.getOption(s)
    def fpu(i: Instrument): Option[String] =
      for {
        mode <- instrumentFPUModeO
          .getOption(s)
          .orElse(FPUMode.BuiltIn.some) // If the instrument has no fpu mode default to built in
        fpuL = if (mode === FPUMode.BuiltIn) instrumentFPUO
        else instrumentFPUCustomMaskO
        fpu <- fpuL.getOption(s)
      } yield fpuNameMapper(i)(fpu).getOrElse(fpu)

    def fpuOrMask(i: Instrument): Option[String] =
      fpu(i)
        .orElse(instrumentSlitWidthO.getOption(s))
        .orElse(instrumentMaskO.getOption(s))

    private def gpiFilter: Step => Option[String] = s => {
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

    def filter(i: Instrument): Option[String] = i match {
      case Instrument.GmosS =>
        instrumentFilterO
          .getOption(s)
          .flatMap(enumerations.filter.GmosSFilter.get)
      case Instrument.GmosN =>
        instrumentFilterO
          .getOption(s)
          .flatMap(enumerations.filter.GmosNFilter.get)
      case Instrument.F2 =>
        instrumentFilterO
          .getOption(s)
          .flatMap(enumerations.filter.F2Filter.get)
      case Instrument.Niri =>
        instrumentFilterO
          .getOption(s)
          .flatMap(enumerations.filter.Niri.get)
      case Instrument.Gnirs =>
        instrumentFilterO
          .getOption(s)
          .map(_.sentenceCase)
      case Instrument.Nifs =>
        instrumentFilterO
          .getOption(s)
          .map(_.sentenceCase)
      case Instrument.Gpi => gpiFilter(s)
      case _              => None
    }

    private def disperserNameMapper(i: Instrument): Map[String, String] =
      i match {
        case Instrument.GmosS => enumerations.disperser.GmosSDisperser
        case Instrument.GmosN => enumerations.disperser.GmosNDisperser
        case Instrument.Gpi   => gpiDispersers
        case _                => Map.empty
      }

    def disperser(i: Instrument): Option[String] = {
      val disperser = for {
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

    def offsetP: TelescopeOffset.P =
      telescopeOffsetPO.getOption(s).getOrElse(TelescopeOffset.P.Zero)
    def offsetQ: TelescopeOffset.Q =
      telescopeOffsetQO.getOption(s).getOrElse(TelescopeOffset.Q.Zero)
    def guiding: Boolean = telescopeGuidingWithT.exist(_ === Guiding.Guide)(s)
    def readMode: Option[String] = instrumentReadModeO.getOption(s)

    def offsetText(axis: OffsetAxis): String =
      offsetValueFormat(axis match {
        case OffsetAxis.AxisP =>
          telescopeOffsetPO.getOption(s).getOrElse(TelescopeOffset.P.Zero)
        case OffsetAxis.AxisQ =>
          telescopeOffsetQO.getOption(s).getOrElse(TelescopeOffset.Q.Zero)
      })

    def offsetPText: String = offsetText(OffsetAxis.AxisP)
    def offsetQText: String = offsetText(OffsetAxis.AxisQ)

    def observingMode: Option[String] =
      instrumentObservingModeO
        .getOption(s)
        .flatMap(obsNames.get)

    def cameraName(i: Instrument): Option[String] = i match {
      case Instrument.Niri =>
        instrumentCameraO
          .getOption(s)
          .flatMap(enumerations.camera.Niri.get)
      case _ => None
    }

    def deckerName: Option[String] =
      instrumentDeckerO.getOption(s)

    def imagingMirrorName: Option[String] =
      instrumentImagingMirrorO.getOption(s)
  }

  implicit class OffsetFnsOps(val steps: List[Step]) extends AnyVal {
    // Calculate if there are non-zero offsets
    def areNonZeroOffsets: Boolean =
      steps
        .map(
          s =>
            telescopeOffsetPO
              .exist(_ =!= TelescopeOffset.P.Zero)(s) || telescopeOffsetQO
              .exist(_ =!= TelescopeOffset.Q.Zero)(s))
        .fold(false)(_ || _)

    // Offsets to be displayed with a width
    def offsetsDisplay: OffsetsDisplay = {
      val (p, q) = steps.sequenceOffsetWidths
      OffsetsDisplay.DisplayOffsets(scala.math.max(p, q))
    }
  }

}
