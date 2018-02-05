// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gmos

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.EpicsCodex.EncodeEpicsValue
import edu.gemini.seqexec.server.gmos.GmosController.Config.{Beam, InBeam, OutOfBeam, ROI}
import edu.gemini.seqexec.server.{EpicsCodex, ObserveCommand, SeqAction, SeqexecFailure}
import edu.gemini.seqexec.server.EpicsUtil.smartSetParam
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpReadMode
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpGain
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpCount
import edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
import edu.gemini.spModel.gemini.gmos.GmosCommonType.Order
import edu.gemini.spModel.gemini.gmos.GmosSouthType.{DisperserSouth => Disperser}
import org.log4s.getLogger
import squants.{Length, Seconds, Time}

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

  implicit val ampCountEncoder: EncodeEpicsValue[AmpCount, String] = EncodeEpicsValue {
    // gmosAmpCount.lut
    case AmpCount.THREE  => ""
    case AmpCount.SIX    => "BEST"
    case AmpCount.TWELVE => "ALL"
  }

  implicit val binningEncoder: EncodeEpicsValue[Binning, Int] = EncodeEpicsValue { b => b.getValue }

  implicit val disperserOrderEncoder: EncodeEpicsValue[DisperserOrder, String] = EncodeEpicsValue(_.sequenceValue)

  implicit val disperserOrderEncoderInt: EncodeEpicsValue[DisperserOrder, Int] = EncodeEpicsValue{
    case Order.ZERO => 0
    case Order.ONE  => 1
    case Order.TWO  => 2
  }

  implicit val disperserLambdaEncoder: EncodeEpicsValue[Length, Double] = EncodeEpicsValue((l: Length) => l.toNanometers)

  implicit val useElectronicOffsetEncoder: EncodeEpicsValue[UseElectronicOffset, Int] = EncodeEpicsValue(_.allow ? 1 | 0)

  private def setShutterState(s: ShutterState): SeqAction[Unit] = s match {
    case UnsetShutter => SeqAction.void
    case s            => DC.setShutterState(encode(s))
  }

  private def roiNumUsed(s: RegionsOfInterest): Int = s.rois match {
    case \/-(rois) => rois.length
    case -\/(_)    => 1
  }

  private def setROI(binning: CCDBinning, s: RegionsOfInterest): SeqAction[Unit] = s.rois match {
    case -\/(b)    => roiParameters(binning, 1, encoders.builtInROI.encode(b))
    case \/-(rois) => rois.zipWithIndex.map { case (roi, i) =>
      roiParameters(binning, i + 1, ROIValues.fromOCS(roi))
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
    _ <- DC.setGainSetting(encoders.autoGain.encode((dc.r.ampReadMode, dc.r.ampGain)))
    _ <- DC.setAmpCount(encode(dc.r.ampCount))
    _ <- DC.setRoiNumUsed(roiNumUsed(dc.roi))
    _ <- setROI(dc.bi, dc.roi)
    _ <- DC.setCcdXBinning(encode(dc.bi.x))
    _ <- DC.setCcdYBinning(encode(dc.bi.y))
  } yield ()

  def setFilters(f: T#Filter): SeqAction[Unit] = {
    val (filter1, filter2) = encoders.filter.encode(f)

    smartSetParam(filter1, GmosEpics.instance.filter1, CC.setFilter1(filter1)) *>
      smartSetParam(filter2, GmosEpics.instance.filter2, CC.setFilter2(filter2))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def setDisperser(d: GmosController.Config[T]#GmosDisperser): SeqAction[Unit] = {
    // TODO: add support for Enum parameters in acm, and then define Enum type for disperserMode
    val disperserMode0 = "WLEN"
    val disperserMode1 = "SEL"
    val disperser = encoders.disperser.encode(d.disperser)
    def disperserModeDecode(v : Int): String = if(v===0) disperserMode0 else disperserMode1

    smartSetParam(disperser.toUpperCase, GmosEpics.instance.disperser.map(_.toUpperCase), CC.setDisperser(disperser)) *>
      smartSetParam(disperserMode0, GmosEpics.instance.disperserMode.map(disperserModeDecode), CC.setDisperserMode(disperserMode0)) *>
      d.order.filter(_ => d.disperser != Disperser.MIRROR).fold(SeqAction.void)(o =>
        smartSetParam(disperserOrderEncoderInt.encode(o), GmosEpics.instance.disperserOrder, CC.setDisperserOrder(disperserOrderEncoder.encode(o)))) *>
      d.lambda.filter(_ => d.disperser != Disperser.MIRROR && !d.order.contains(Order.ZERO)).fold(SeqAction.void)(o =>
        smartSetParam(encode(o), GmosEpics.instance.disperserWavel, CC.setDisperserLambda(encode(o))) )

  }

  def setFPU(cc: GmosFPU): SeqAction[Unit] = {
    def inBeamDecode(v: Int): String = if (v===0) InBeamVal else OutOfBeamVal

    def builtInFPU(fpu: T#FPU): (Option[String], Option[String]) = encoders.fpu.encode(fpu)

    def customFPU(name: String): (Option[String], Option[String]) = name match {
      case "None" => (none, none)
      case _      => (name.some, beamEncoder.encode(InBeam).some)
    }

    def setFPUParams(p: (Option[String], Option[String])): SeqAction[Unit] = p match {
      case (fpuName, beam) =>
        fpuName.fold(SeqAction.void)(v => smartSetParam(v, GmosEpics.instance.fpu, CC.setFpu(v))) *>
          beam.fold(SeqAction.void)(v => smartSetParam(v, GmosEpics.instance.inBeam.map(inBeamDecode), CC.setInBeam(v)))
    }

    cc match {
      case cfg.BuiltInFPU(fpu) => setFPUParams(builtInFPU(fpu))
      case CustomMaskFPU(name) => setFPUParams(customFPU(name))
      case UnknownFPU          => SeqAction.void
      case _                   => SeqAction.fail(SeqexecFailure.Unexpected("Failed match on built-in FPU"))
    }
  }

  private val PixelsToMicrons = 15.0

  def setCCConfig(cc: GmosController.Config[T]#CCConfig): SeqAction[Unit] = {
    val stage = encoders.stageMode.encode(cc.stage)
    val ElectronicOffsetOff = 0

    for {
      _ <- setFilters(cc.filter)
      _ <- setDisperser(cc.disperser)
      _ <- setFPU(cc.fpu)
      _ <- smartSetParam(stage, GmosEpics.instance.stageMode, CC.setStageMode(stage))
      _ <- CC.setDtaXOffset(cc.dtaX.intValue.toDouble * PixelsToMicrons)
      _ <- cc.useElectronicOffset.fold(CC.setElectronicOffsetting(ElectronicOffsetOff))(e =>
        smartSetParam(e.allow, GmosEpics.instance.useElectronicOffsetting, CC.setElectronicOffsetting(encode(e))))
    } yield ()
  }

  override def applyConfig(config: GmosController.GmosConfig[T]): SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Start Gmos configuration").right))
    _ <- setDCConfig(config.dc)
    _ <- setCCConfig(config.cc)
    _ <- GmosEpics.instance.configCmd.setTimeout(ConfigTimeout)
    _ <- GmosEpics.instance.post
    _ <- EitherT(Task(Log.info("Completed Gmos configuration").right))
  } yield ()

  override def observe(obsid: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] = for {
    _ <- EitherT(Task(Log.info("Start Gmos observation").right))
    _ <- GmosEpics.instance.observeCmd.setLabel(obsid)
    _ <- GmosEpics.instance.observeCmd.setTimeout(expTime+ReadoutTimeout)
    ret <- GmosEpics.instance.observeCmd.post
    _ <- EitherT(Task(Log.info("Completed Gmos observation").right))
  } yield ret

  override def stopObserve: SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Stop Gmos exposure").right))
    _ <- GmosEpics.instance.stopCmd.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.stopCmd.mark
    _ <- GmosEpics.instance.stopCmd.post
  } yield ()

  override def abortObserve: SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Abort Gmos exposure").right))
    _ <- GmosEpics.instance.abortCmd.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.abortCmd.mark
    _ <- GmosEpics.instance.abortCmd.post
  } yield ()

  override def endObserve: SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Send endObserve to Gmos").right))
    _ <- GmosEpics.instance.endObserveCmd.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.endObserveCmd.mark
    _ <- GmosEpics.instance.endObserveCmd.post
  } yield ()

  override def pauseObserve = for {
    _ <- EitherT(Task(Log.info("Send pause to Gmos").right))
    _ <- GmosEpics.instance.pauseCmd.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.pauseCmd.mark
    _ <- GmosEpics.instance.pauseCmd.post
  } yield ()

  override def resumePaused(expTime: Time): SeqAction[ObserveCommand.Result] = for {
    _ <- EitherT(Task(Log.info("Resume Gmos observation").right))
    _ <- GmosEpics.instance.continueCmd.setTimeout(expTime+ReadoutTimeout)
    _ <- GmosEpics.instance.continueCmd.mark
    ret <- GmosEpics.instance.continueCmd.post
    _ <- EitherT(Task(Log.info("Completed Gmos observation").right))
  } yield ret

  override def stopPaused = for {
    _ <- EitherT(Task(Log.info("Stop Gmos paused observation").right))
    _ <- GmosEpics.instance.stopAndWaitCmd.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.stopAndWaitCmd.mark
    ret <- GmosEpics.instance.stopAndWaitCmd.post
    _ <- EitherT(Task(Log.info("Completed stopping Gmos observation").right))
  } yield if(ret === ObserveCommand.Success) ObserveCommand.Stopped else ret

  override def abortPaused = for {
    _ <- EitherT(Task(Log.info("Abort Gmos paused observation").right))
    _ <- GmosEpics.instance.abortAndWait.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.abortAndWait.mark
    ret <- GmosEpics.instance.abortAndWait.post
    _ <- EitherT(Task(Log.info("Completed aborting Gmos observation").right))
  } yield if(ret === ObserveCommand.Success) ObserveCommand.Aborted else ret
}

object GmosControllerEpics {
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
    def fromInt(xStart: Int, xSize: Int, yStart: Int, ySize: Int): Option[ROIValues] =
      (XStart.fromInt(xStart) |@| XSize.fromInt(xSize) |@| YStart.fromInt(yStart) |@| YSize.fromInt(ySize))(new ROIValues(_, _, _, _) {})

    // Built from OCS ROI values
    def fromOCS(roi: ROI): Option[ROIValues] =
      (XStart.fromInt(roi.getXStart) |@| XSize.fromInt(roi.getXSize) |@| YStart.fromInt(roi.getYStart) |@| YSize.fromInt(roi.getYSize))(new ROIValues(_, _, _, _) {})

  }

  trait Encoders[T<:GmosController.SiteDependentTypes] {
    val filter: EncodeEpicsValue[T#Filter, (String, String)]
    val fpu: EncodeEpicsValue[T#FPU, (Option[String], Option[String])]
    val stageMode: EncodeEpicsValue[T#GmosStageMode, String]
    val disperser: EncodeEpicsValue[T#Disperser, String]
    val builtInROI: EncodeEpicsValue[BuiltinROI, Option[ROIValues]]
    val autoGain: EncodeEpicsValue[(AmpReadMode, AmpGain), Int]
  }

  val InBeamVal: String = "IN-BEAM"
  val OutOfBeamVal: String = "OUT-OF-BEAM"
  implicit val beamEncoder: EncodeEpicsValue[Beam, String] = EncodeEpicsValue {
    case OutOfBeam => OutOfBeamVal
    case InBeam    => InBeamVal
  }

  val DefaultTimeout: Time = Seconds(60)
  val ReadoutTimeout: Time = Seconds(300)
  val ConfigTimeout: Time = Seconds(600)

}
