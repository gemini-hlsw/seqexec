// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gmos

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.EpicsCodex._
import edu.gemini.seqexec.server.gmos.GmosController.Config.{Beam, InBeam, OutOfBeam}
import edu.gemini.seqexec.server.{EpicsCodex, ObserveCommand, SeqAction, SeqexecFailure}
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpReadMode
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpGain
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpCount
import edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
import edu.gemini.spModel.gemini.gmos.GmosCommonType.Order
import edu.gemini.spModel.gemini.gmos.GmosSouthType.{DisperserSouth => Disperser}
import org.log4s.getLogger
import squants.Length

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

class GmosControllerEpics[T<:GmosController.SiteDependentTypes](encoders: GmosControllerEpics.Encoders[T])(cfg: GmosController.Config[T]) extends GmosController[T] {
  private val Log = getLogger

  import EpicsCodex._
  import GmosController.Config._
  import GmosControllerEpics._

  private val CC = GmosEpics.instance.configCmd
  private val DC = GmosEpics.instance.configDCCmd

  override def getConfig: SeqAction[GmosController.GmosConfig[T]] = ??? // scalastyle:ignore

  implicit val ampReadModeEncoder: EncodeEpicsValue[AmpReadMode, String] = EncodeEpicsValue {
    case AmpReadMode.SLOW => "SLOW"
    case AmpReadMode.FAST => "FAST"
  }

  implicit val shutterStateEncoder: EncodeEpicsValue[ShutterState, String] = EncodeEpicsValue {
    case OpenShutter  => "OPEN"
    case CloseShutter => "CLOSED"
    case _            => ""
  }

  implicit val ampGainSettingEncoder: EncodeEpicsValue[AmpGainSetting, String] = EncodeEpicsValue(_.value.toString)

  implicit val ampCountEncoder: EncodeEpicsValue[AmpCount, String] = EncodeEpicsValue {
    // gmosAmpCount.lut
    case AmpCount.THREE  => ""
    case AmpCount.SIX    => "BEST"
    case AmpCount.TWELVE => "ALL"
  }

  implicit val binningEncoder: EncodeEpicsValue[Binning, Int] = EncodeEpicsValue { b => b.getValue }

  implicit val disperserOrderEncoder: EncodeEpicsValue[DisperserOrder, String] = EncodeEpicsValue(_.sequenceValue)

  implicit val disperserLambdaEncoder: EncodeEpicsValue[Length, Double] = EncodeEpicsValue((l: Length) => l.toNanometers)

  implicit val useElectronicOffsetEncoder: EncodeEpicsValue[UseElectronicOffset, Int] = EncodeEpicsValue(_.allow ? 1 | 0)

  private def gainSetting(ampMode: AmpReadMode, ampGain: AmpGain): AmpGainSetting = (ampMode, ampGain) match {
    // gmosAutoGain.lut
    case (AmpReadMode.SLOW, AmpGain.LOW)  => AmpGainSetting(0)
    case (AmpReadMode.SLOW, AmpGain.HIGH) => AmpGainSetting(0)
    case (AmpReadMode.FAST, AmpGain.LOW)  => AmpGainSetting(10)
    case (AmpReadMode.FAST, AmpGain.HIGH) => AmpGainSetting(0)
  }

  private def setShutterState(s: ShutterState): SeqAction[Unit] = s match {
    case UnsetShutter => SeqAction.void
    case s            => DC.setShutterState(encode(s))
  }

  private def roiNumUsed(s: RegionsOfInterest): Int = s.rois match {
    case \/-(rois) => rois.length
    case -\/(_)    => 1
  }

  // Parameters to define a ROI
  sealed abstract case class XStart(value: Int)
  // Make the values impossible to build with invalid values
  object XStart {
    def fromInt(v: Int): Option[XStart] = (v > 0) option new XStart(v) {}
  }

  sealed abstract case class XSize(value: Int)
  object XSize {
    def fromInt(v: Int): Option[XSize] = (v > 0) option new XSize(v) {}
  }

  sealed abstract case class YStart(value: Int)
  object YStart {
    def fromInt(v: Int): Option[YStart] = (v > 0) option new YStart(v) {}
  }

  sealed abstract case class YSize(value: Int)
  object YSize {
    def fromInt(v: Int): Option[YSize] = (v > 0) option new YSize(v) {}
  }

  sealed abstract case class ROIValues(xStart: XStart, xSize: XSize, yStart: YStart, ySize: YSize)

  object ROIValues {
    // Build out of fixed values, I wish this could be constrained a bit more
    // but these are hardcoded values according to LUTS
    // Being private we ensure it is mostly sane
    private def fromInt(xStart: Int, xSize: Int, yStart: Int, ySize: Int): Option[ROIValues] =
      (XStart.fromInt(xStart) |@| XSize.fromInt(xSize) |@| YStart.fromInt(yStart) |@| YSize.fromInt(ySize))(new ROIValues(_, _, _, _) {})

    // Built from OCS ROI values
    def fromOCS(roi: ROI): Option[ROIValues] =
      (XStart.fromInt(roi.getXStart) |@| XSize.fromInt(roi.getXSize) |@| YStart.fromInt(roi.getYStart) |@| YSize.fromInt(roi.getYSize))(new ROIValues(_, _, _, _) {})

    def builtInROI(b: BuiltinROI): Option[ROIValues] = b match {
      // gmosROI.lut
      case BuiltinROI.FULL_FRAME       => ROIValues.fromInt(xStart = 1, xSize = 6144, yStart = 1, ySize = 4224)
      case BuiltinROI.CCD2             => ROIValues.fromInt(xStart = 2049, xSize = 2048, yStart = 1, ySize = 4224)
      case BuiltinROI.CENTRAL_SPECTRUM => ROIValues.fromInt(xStart = 1, xSize = 6144, yStart = 1625, ySize = 1024)
      case BuiltinROI.CENTRAL_STAMP    => ROIValues.fromInt(xStart = 2923, xSize = 300, yStart = 1987, ySize = 300)
      case _                           => None
    }
  }

  private def setROI(binning: CCDBinning, s: RegionsOfInterest): SeqAction[Unit] = s.rois match {
    case -\/(b)    => roiParameters(binning, 1, ROIValues.builtInROI(b))
    case \/-(rois) => rois.zipWithIndex.map { case (roi, i) =>
      roiParameters(binning, i, ROIValues.fromOCS(roi))
    }.sequenceU.flatMap(_ => SeqAction.void)
  }

  private def roiParameters(binning: CCDBinning, index: Int, roi: Option[ROIValues]): SeqAction[Unit] = {
    (roi |@| DC.rois.get(index)) { (roi, r) =>
      for {
        _ <- r.setCcdXstart1(roi.xStart.value)
        _ <- r.setCcdXsize1(roi.xSize.value / binning.x.getValue)
        _ <- r.setCcdYstart1(roi.yStart.value)
        _ <- r.setCcdYsize1(roi.ySize.value / binning.y.getValue)
      } yield ()
    }.fold(SeqAction.void)(identity)
  }

  def setDCConfig(dc: DCConfig): SeqAction[Unit] = for {
    // TODO nsRow, nsPairs
    _ <- DC.setExposureTime(dc.t)
    _ <- setShutterState(dc.s)
    _ <- DC.setAmpReadMode(encode(dc.r.ampReadMode))
    _ <- DC.setGainSetting(encode(gainSetting(dc.r.ampReadMode, dc.r.ampGain)))
    _ <- DC.setAmpCount(encode(dc.r.ampCount))
    _ <- DC.setRoiNumUsed(roiNumUsed(dc.roi))
    _ <- setROI(dc.bi, dc.roi)
    _ <- DC.setCcdXBinning(encode(dc.bi.x))
    _ <- DC.setCcdYBinning(encode(dc.bi.y))
  } yield ()



  def setFilters(f: T#Filter): SeqAction[Unit] = {
    val (filter1, filter2) = encoders.filter.encode(f)

    CC.setFilter1(filter1) *> CC.setFilter2(filter2)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def setDisperser(d: GmosController.Config[T]#GmosDisperser): SeqAction[Unit] = {
    // TODO: add support for Enum parameters in acm, and then define Enum type for disperserMode
    val disperserMode = "WLEN"
    CC.setDisperser(encoders.disperser.encode(d.disperser)) *>
      CC.setDisperserMode(disperserMode) *>
      d.order.filter(_ => d.disperser != Disperser.MIRROR).fold(SeqAction.void)(o => CC.setDisperserOrder(encode(o))) *>
      d.lambda.filter(_ => d.disperser != Disperser.MIRROR && !d.order.contains(Order.ZERO)).fold(SeqAction.void)(o => CC.setDisperserLambda(encode(o)))
  }

  def setFPU(cc: GmosFPU): SeqAction[Unit] = {
    def builtInFPU(fpu: T#FPU): SeqAction[Unit] = {
      val (fpuName, beam) = encoders.fpu.encode(fpu)

      fpuName.fold(SeqAction.void)(CC.setFpu) *>
        beam.fold(SeqAction.void)(CC.setInBeam)
    }

    def customFPU(name: String): SeqAction[Unit] = {
      val (fpuName, beam: Option[Beam]) = name match {
        case "None" => (none, none)
        case _      => (name.some, InBeam.some)
      }
      fpuName.fold(SeqAction.void)(CC.setFpu) *>
        beam.fold(SeqAction.void)(b => CC.setInBeam(beamEncoder.encode(b)))
    }

    cc match {
      case cfg.BuiltInFPU(fpu) => builtInFPU(fpu)
      case CustomMaskFPU(name) => customFPU(name)
      case UnknownFPU          => SeqAction.void
      case _                   => SeqAction.fail(SeqexecFailure.Unexpected("Failed match on built-in FPU"))
    }
  }

  private val PixelsToMicrons = 15.0

  def setCCConfig(cc: GmosController.Config[T]#CCConfig): SeqAction[Unit] = for {
    _ <- setFilters(cc.filter)
    _ <- setDisperser(cc.disperser)
    _ <- setFPU(cc.fpu)
    _ <- CC.setStageMode(encoders.stageMode.encode(cc.stage))
    _ <- CC.setDtaXOffset(cc.dtaX.intValue.toDouble*PixelsToMicrons)
    _ <- cc.useElectronicOffset.fold(CC.setElectronicOffsetting(0))(e => CC.setElectronicOffsetting(encode(e)))
  } yield ()

  // TODO: Deal with Pause
  override def applyConfig(config: GmosController.GmosConfig[T]): SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Start Gmos configuration").right))
    _ <- setDCConfig(config.dc)
    _ <- setCCConfig(config.cc)
    _ <- GmosEpics.instance.post
    _ <- EitherT(Task(Log.info("Completed Gmos configuration").right))
  } yield ()

  override def observe(obsid: ImageFileId): SeqAction[ObserveCommand.Result] = for {
    _ <- EitherT(Task(Log.info("Start Gmos observation").right))
    _ <- GmosEpics.instance.observeCmd.setLabel(obsid)
    ret <- GmosEpics.instance.observeCmd.post
    _ <- EitherT(Task(Log.info("Completed Gmos observation").right))
  } yield ret

  override def stopObserve: SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Stop Gmos exposure").right))
    _ <- GmosEpics.instance.stopCmd.mark
    _ <- GmosEpics.instance.stopCmd.post
  } yield ()

  override def abortObserve: SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Abort Gmos exposure").right))
    _ <- GmosEpics.instance.abortCmd.mark
    _ <- GmosEpics.instance.abortCmd.post
  } yield ()

  override def endObserve: SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Send endObserve to Gmos").right))
    _ <- GmosEpics.instance.endObserveCmd.mark
    _ <- GmosEpics.instance.endObserveCmd.post
  } yield ()
}

object GmosControllerEpics {
  trait Encoders[T<:GmosController.SiteDependentTypes] {
    val filter: EncodeEpicsValue[T#Filter, (String, String)]
    val fpu: EncodeEpicsValue[T#FPU, (Option[String], Option[String])]
    val stageMode: EncodeEpicsValue[T#GmosStageMode, String]
    val disperser: EncodeEpicsValue[T#Disperser, String]
  }

  implicit val beamEncoder: EncodeEpicsValue[Beam, String] = EncodeEpicsValue {
    case OutOfBeam => "OUT-OF-BEAM"
    case InBeam    => "IN-BEAM"
  }

}
