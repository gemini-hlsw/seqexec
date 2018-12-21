// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import edu.gemini.spModel.gemini.gmos.GmosCommonType._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.UseElectronicOffsettingRuling
import fs2.Stream
import mouse.all._
import org.log4s.getLogger
import seqexec.model.dhs.ImageFileId
import seqexec.server.EpicsCodex.EncodeEpicsValue
import seqexec.server.EpicsUtil.{smartSetDoubleParam, smartSetParam}
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.{EpicsCodex, EpicsUtil, ObserveCommand, Progress, SeqAction, SeqexecFailure}
import seqexec.server.gmos.GmosController.Config.{Beam, InBeam, OutOfBeam, ROI}
import squants.time.TimeConversions._
import squants.{Length, Seconds, Time}

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

  implicit val disperserLambdaEncoder: EncodeEpicsValue[Length, Double] =
    EncodeEpicsValue((l: Length) => l.toNanometers)

  implicit val useElectronicOffsetEncoder: EncodeEpicsValue[UseElectronicOffset, Int] =
    EncodeEpicsValue(_.allow.fold(1, 0))

  private def setShutterState(s: ShutterState): List[SeqAction[Unit]] = s match {
    case UnsetShutter => List.empty
    case sh           => {
      val encodedVal = encode(sh)
      smartSetParam(encodedVal, GmosEpics.instance.shutterState, DC.setShutterState(encode(sh)))
    }
  }

  private def roiNumUsed(s: RegionsOfInterest): Int = s.rois match {
    case Right(rois) => rois.length
    case Left(_)     => 1
  }

  private def setROI(binning: CCDBinning, s: RegionsOfInterest): List[SeqAction[Unit]] =
    s.rois match {
      case Left(b)     => roiParameters(binning, 1, encoders.builtInROI.encode(b))
      case Right(rois) => rois.zipWithIndex.flatMap { case (roi, i) =>
        roiParameters(binning, i + 1, ROIValues.fromOCS(roi))
      }
    }

  private def roiParameters(binning: CCDBinning, index: Int, roi: Option[ROIValues])
  : List[SeqAction[Unit]] = (roi, DC.rois.get(index)).mapN { (roi, r) =>
    r.setCcdXstart1(roi.xStart.value) *>
    r.setCcdXsize1(roi.xSize.value / binning.x.getValue) *>
    r.setCcdYstart1(roi.yStart.value) *>
    r.setCcdYsize1(roi.ySize.value / binning.y.getValue)
  }.toList

  private def setExposureTime(t: ExposureTime): List[SeqAction[Unit]] =
    smartSetParam(t.toSeconds.toInt, GmosEpics.instance.reqExposureTime, DC.setExposureTime(t))

  private def setAmpReadMode(v: AmpReadMode): List[SeqAction[Unit]] = {
    val encodedVal = encode(v)
    smartSetParam(encodedVal, GmosEpics.instance.ampReadMode, DC.setAmpReadMode(encodedVal))
  }

  private def setGainSetting(rm: AmpReadMode, g: AmpGain): List[SeqAction[Unit]] = {
    val encodedVal = encoders.autoGain.encode((rm, g))
    smartSetParam(encodedVal, GmosEpics.instance.gainSetting, DC.setGainSetting(encodedVal))
  }

  private def setAmpCount(v: AmpCount): List[SeqAction[Unit]] = {
    val encodedVal = encode(v)
    smartSetParam(encodedVal, GmosEpics.instance.ampCount, DC.setAmpCount(encodedVal))
  }

  private def setRoiNumUsed(n: Int): List[SeqAction[Unit]] =
    smartSetParam(n, GmosEpics.instance.roiNumUsed, DC.setRoiNumUsed(n))

  private def setCcdXBinning(v: Binning): List[SeqAction[Unit]] = {
    val encodedVal = encode(v)
    smartSetParam(encodedVal, GmosEpics.instance.ccdXBinning, DC.setCcdXBinning(encodedVal))
  }

  private def setCcdYBinning(v: Binning): List[SeqAction[Unit]] = {
    val encodedVal = encode(v)
    smartSetParam(encodedVal, GmosEpics.instance.ccdYBinning, DC.setCcdYBinning(encodedVal))
  }

  def setDCConfig(dc: DCConfig): List[SeqAction[Unit]] =
    // TODO nsRow, nsPairs
    setExposureTime(dc.t) ++
    setShutterState(dc.s) ++
    setAmpReadMode(dc.r.ampReadMode) ++
    setGainSetting(dc.r.ampReadMode, dc.r.ampGain) ++
    setAmpCount(dc.r.ampCount) ++
    setRoiNumUsed(roiNumUsed(dc.roi)) ++
    setROI(dc.bi, dc.roi) ++
    setCcdXBinning(dc.bi.x) ++
    setCcdYBinning(dc.bi.y)

  def setFilters(f: T#Filter): List[SeqAction[Unit]] = {
    val (filter1, filter2) = encoders.filter.encode(f)

    smartSetParam(filter1, GmosEpics.instance.filter1, CC.setFilter1(filter1)) ++
      smartSetParam(filter2, GmosEpics.instance.filter2, CC.setFilter2(filter2))
  }

  def setDisperser(d: T#Disperser): List[SeqAction[Unit]] = {
    val encodedVal = encoders.disperser.encode(d)
    val s = smartSetParam(encodedVal.toUpperCase,
      GmosEpics.instance.disperser.map(_.toUpperCase),
      CC.setDisperser(encodedVal)
    )
    // Force setting if disperser is parked
    if (s.isEmpty)
      smartSetParam(false, GmosEpics.instance.disperserParked, CC.setDisperser(encodedVal))
    else s
  }

  def setOrder(o: DisperserOrder): List[SeqAction[Unit]] = smartSetParam(
    disperserOrderEncoderInt.encode(o), GmosEpics.instance.disperserOrder,
    CC.setDisperserOrder(disperserOrderEncoder.encode(o))
  )

  def setDisperserParams(d: GmosController.Config[T]#GmosDisperser): List[SeqAction[Unit]] = {
    // TODO: define Enum type for disperserMode
    val disperserMode0 = "WLEN"
    val disperserMode1 = "SEL"

    def disperserModeDecode(v: Int): String = if (v === 0) disperserMode0 else disperserMode1

    val set = d match {
      case cfg.GmosDisperser.Mirror =>
        val s = setDisperser(cfg.mirror)
        //If disperser is set, force mode configuration
        if (s.isEmpty) Nil else s ++ List(CC.setDisperserMode(disperserMode0))
      case cfg.GmosDisperser.Order0(d) =>
        val s0 = setDisperser(d)
        // If disperser is set, force order configuration
        val s = if(s0.isEmpty) setOrder(Order.ZERO)
                else CC.setDisperserOrder(disperserOrderEncoder.encode(Order.ZERO)) :: s0
        //If disperser or order are set, force mode configuration
        if (s.isEmpty) Nil else s ++ List(CC.setDisperserMode(disperserMode0))
      case cfg.GmosDisperser.OrderN(d, o, w) =>
        val s0 = setDisperser(d)
        val s = (if(s0.isEmpty) setOrder(o)
                else CC.setDisperserOrder(disperserOrderEncoder.encode(o)) :: s0) ++
                smartSetParam(encode(w), GmosEpics.instance.disperserWavel,
                  CC.setDisperserLambda(encode(w))
        )
        //If disperser, order or wavelength are set, force mode configuration
        if (s.isEmpty) Nil else s ++ List(CC.setDisperserMode(disperserMode0))
      //TODO Improve data model to remove this case. It is here because search includes types of
      // both sites.
      case _                   => List.empty
    }
    if(set.isEmpty)
      smartSetParam(disperserMode0, GmosEpics.instance.disperserMode.map(disperserModeDecode),
        CC.setDisperserMode(disperserMode0))
    else set
  }

  def setFPU(cc: GmosFPU): List[SeqAction[Unit]] = {
    def inBeamDecode(v: Int): String = if (v===0) InBeamVal else OutOfBeamVal

    def builtInFPU(fpu: T#FPU): (Option[String], Option[String]) = encoders.fpu.encode(fpu)

    def customFPU(name: String): (Option[String], Option[String]) = name match {
      case "None" => (none, none)
      case _      => (name.some, beamEncoder.encode(InBeam).some)
    }

    def setFPUParams(p: (Option[String], Option[String])): List[SeqAction[Unit]] = p match {
      case (fpuName, beam) =>
        fpuName.map(v => smartSetParam(v, GmosEpics.instance.fpu, CC.setFpu(v))).toList.flatten ++
          beam.map(v => smartSetParam(v, GmosEpics.instance.inBeam.map(inBeamDecode), CC.setInBeam
          (v))).toList.flatten
    }

    cc match {
      case cfg.BuiltInFPU(fpu) => setFPUParams(builtInFPU(fpu))
      case CustomMaskFPU(name) => setFPUParams(customFPU(name))
      case UnknownFPU          => List.empty
      //TODO Improve data model to remove this case. It is here because the BuiltInFPU of the
      // other site is also a type of GmosFPU, even if it never will appear here.
      case _                   => List.empty
    }
  }

  def setElectronicOffset(e: Option[UseElectronicOffset]): List[SeqAction[Unit]] = {
    val useEOffset = e.getOrElse(UseElectronicOffsettingRuling.deny(""))

    smartSetParam(useEOffset.allow, GmosEpics.instance.useElectronicOffsetting,
      CC.setElectronicOffsetting(encode(useEOffset)))
  }

  def setStage(v: T#GmosStageMode): List[SeqAction[Unit]] = {
    val stage = encoders.stageMode.encode(v)

    smartSetParam(stage, GmosEpics.instance.stageMode, CC.setStageMode(stage))
  }

  def setDtaXOffset(v: DTAX): List[SeqAction[Unit]] = {
    val PixelsToMicrons = 15.0
    val Tolerance = 0.001

    val offsetInMicrons =  v.intValue.toDouble * PixelsToMicrons

    // It seems that the reported dtaXOffset is absolute, but the applied offset is relative to
    // XCenter value
    smartSetDoubleParam(Tolerance)(offsetInMicrons,
      (GmosEpics.instance.dtaXOffset, GmosEpics.instance.dtaXCenter)mapN(_-_),
      CC.setDtaXOffset(offsetInMicrons))
  }

  def setCCConfig(cc: GmosController.Config[T]#CCConfig): List[SeqAction[Unit]] = {
    setFilters(cc.filter) ++
      setDisperserParams(cc.disperser) ++
      setFPU(cc.fpu) ++
      setStage(cc.stage) ++
      setDtaXOffset(cc.dtaX) ++
      setElectronicOffset(cc.useElectronicOffset)
  }

  override def applyConfig(config: GmosController.GmosConfig[T]): SeqAction[Unit] = {
    val params = setDCConfig(config.dc) ++ setCCConfig(config.cc)

    EitherT.right[SeqexecFailure](IO(Log.info("Start Gmos configuration"))) *>
    EitherT.right(IO(Log.debug(s"Gmos configuration: ${config.show}"))) *>
    (if(params.isEmpty) SeqAction.void
    else params.sequence *>
      GmosEpics.instance.configCmd.setTimeout(ConfigTimeout) *>
      GmosEpics.instance.post
    ) *>
    EitherT.right(IO(Log.info("Completed Gmos configuration")))
  }

  override def observe(fileId: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] = for {
    _   <- GmosEpics.instance.observeCmd.setLabel(fileId)
    _   <- GmosEpics.instance.observeCmd.setTimeout(expTime + ReadoutTimeout)
    ret <- GmosEpics.instance.observeCmd.post
  } yield ret

  override def stopObserve: SeqAction[Unit] = for {
    _ <- EitherT.right(IO(Log.info("Stop Gmos exposure")))
    _ <- GmosEpics.instance.stopCmd.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.stopCmd.mark
    _ <- GmosEpics.instance.stopCmd.post
  } yield ()

  override def abortObserve: SeqAction[Unit] = for {
    _ <- EitherT.right(IO(Log.info("Abort Gmos exposure")))
    _ <- GmosEpics.instance.abortCmd.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.abortCmd.mark
    _ <- GmosEpics.instance.abortCmd.post
  } yield ()

  override def endObserve: SeqAction[Unit] = for {
    _ <- EitherT.right(IO(Log.debug("Send endObserve to Gmos")))
    _ <- GmosEpics.instance.endObserveCmd.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.endObserveCmd.mark
    _ <- GmosEpics.instance.endObserveCmd.post
  } yield ()

  override def pauseObserve: SeqAction[Unit] = for {
    _ <- EitherT.right(IO(Log.info("Send pause to Gmos")))
    _ <- GmosEpics.instance.pauseCmd.setTimeout(DefaultTimeout)
    _ <- GmosEpics.instance.pauseCmd.mark
    _ <- GmosEpics.instance.pauseCmd.post
  } yield ()

  override def resumePaused(expTime: Time): SeqAction[ObserveCommand.Result] = for {
    _   <- EitherT.right(IO(Log.debug("Resume Gmos observation")))
    _   <- GmosEpics.instance.continueCmd.setTimeout(expTime+ReadoutTimeout)
    _   <- GmosEpics.instance.continueCmd.mark
    ret <- GmosEpics.instance.continueCmd.post
    _   <- EitherT.right(IO(Log.debug("Completed Gmos observation")))
  } yield ret

  override def stopPaused: SeqAction[ObserveCommand.Result] = for {
    _   <- EitherT.right(IO(Log.info("Stop Gmos paused observation")))
    _   <- GmosEpics.instance.stopAndWaitCmd.setTimeout(DefaultTimeout)
    _   <- GmosEpics.instance.stopAndWaitCmd.mark
    ret <- GmosEpics.instance.stopAndWaitCmd.post
    _   <- EitherT.right(IO(Log.info("Completed stopping Gmos observation")))
  } yield if(ret === ObserveCommand.Success) ObserveCommand.Stopped else ret

  override def abortPaused: SeqAction[ObserveCommand.Result] = for {
    _   <- EitherT.right(IO(Log.info("Abort Gmos paused observation")))
    _   <- GmosEpics.instance.abortAndWait.setTimeout(DefaultTimeout)
    _   <- GmosEpics.instance.abortAndWait.mark
    ret <- GmosEpics.instance.abortAndWait.post
    _   <- EitherT.right(IO(Log.info("Completed aborting Gmos observation")))
  } yield if(ret === ObserveCommand.Success) ObserveCommand.Aborted else ret

  override def observeProgress(total: Time, elapsed: ElapsedTime): Stream[IO, Progress] =
    EpicsUtil.countdown[IO](total, IO(GmosEpics.instance.countdown.map(_.seconds)),
      IO(GmosEpics.instance.observeState))
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
      (XStart.fromInt(xStart), XSize.fromInt(xSize), YStart.fromInt(yStart), YSize.fromInt(ySize)).mapN(new ROIValues(_, _, _, _) {})

    // Built from OCS ROI values
    def fromOCS(roi: ROI): Option[ROIValues] =
      (XStart.fromInt(roi.getXStart), XSize.fromInt(roi.getXSize), YStart.fromInt(roi.getYStart), YSize.fromInt(roi.getYSize)).mapN(new ROIValues(_, _, _, _) {})

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
