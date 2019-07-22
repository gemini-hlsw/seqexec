// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.effect.{ IO, Timer }
import cats.implicits._
import edu.gemini.spModel.gemini.gmos.GmosCommonType._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.UseElectronicOffsettingRuling
import fs2.Stream
import mouse.all._
import scala.concurrent.ExecutionContext
import org.log4s.getLogger
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.EpicsCodex.EncodeEpicsValue
import seqexec.server.EpicsUtil._
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.{EpicsUtil, Progress, SeqexecFailure}
import seqexec.server.EpicsCodex._
import seqexec.server.gmos.GmosController.Config._
import seqexec.server.gmos.GmosController._
import squants.time.TimeConversions._
import squants.{Length, Seconds, Time}

trait GmosEncoders {
  implicit val ampReadModeEncoder: EncodeEpicsValue[AmpReadMode, String] = EncodeEpicsValue {
    case AmpReadMode.SLOW => "SLOW"
    case AmpReadMode.FAST => "FAST"
  }

  implicit val shutterStateEncoder: EncodeEpicsValue[ShutterState, String] = EncodeEpicsValue {
    case ShutterState.OpenShutter  => "OPEN"
    case ShutterState.CloseShutter => "CLOSED"
    case _                         => ""
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

  implicit val exposureTimeEncoder: EncodeEpicsValue[ExposureTime, Int] = EncodeEpicsValue(_.toSeconds.toInt)

  implicit val disperserLambdaEncoder: EncodeEpicsValue[Length, Double] =
    EncodeEpicsValue((l: Length) => l.toNanometers)

  implicit val useElectronicOffsetEncoder: EncodeEpicsValue[UseElectronicOffset, Int] =
    EncodeEpicsValue(_.allow.fold(1, 0))

  val InBeamVal: String    = "IN-BEAM"
  val OutOfBeamVal: String = "OUT-OF-BEAM"
  implicit val beamEncoder: EncodeEpicsValue[Beam, String] = EncodeEpicsValue {
    case Beam.OutOfBeam => OutOfBeamVal
    case Beam.InBeam    => InBeamVal
  }

  def inBeamDecode(v: Int): String =
    if (v === 0) InBeamVal else OutOfBeamVal

  // TODO: define Enum type for disperserMode
  val disperserMode0 = "WLEN"
  val disperserMode1 = "SEL"

  def disperserModeDecode(v: Int): String = if (v === 0) disperserMode0 else disperserMode1

}

private[gmos] final case class GmosDCEpicsState(
  shutterState: String,
  reqExposureTime: Int,
  ampReadMode: String,
  gainSetting: Int,
  ampCount: String,
  roiNumUsed: Int,
  ccdXBinning: Int,
  ccdYBinning: Int
)

private[gmos] final case class GmosNSEpicsState(
  nsPairs: Int
)

private[gmos] final case class GmosCCEpicsState(
  filter1: String,
  filter2: String,
  disperserMode: Int,
  disperser: String,
  disperserParked: Boolean,
  disperserOrder: Int,
  disperserWavel: Double,
  fpu: String,
  inBeam: Int,
  stageMode: String,
  dtaXOffset: Double,
  dtaXCenter: Double,
  useElectronicOffsetting: Boolean
)

/**
 * Captures the current epics state of GMOS
 */
private[gmos] final case class GmosEpicsState(
  dc: GmosDCEpicsState,
  cc: GmosCCEpicsState,
  ns: GmosNSEpicsState
)

object GmosControllerEpics extends GmosEncoders {
  private val Log = getLogger

  // TODO make this a variable
  private val sys = GmosEpics.instance

  private val CC = sys.configCmd
  private val DC = sys.configDCCmd

  // Read the current state of the
  private def retrieveState: IO[GmosEpicsState] =
    for {
      dc <- retrieveDCState
      cc <- retrieveCCState
      ns <- retrieveNSState
    } yield GmosEpicsState(dc, cc, ns)

  private def retrieveNSState: IO[GmosNSEpicsState] =
    for {
      nsPairs <- sys.nsPairs
    } yield GmosNSEpicsState(nsPairs)

  private def retrieveDCState: IO[GmosDCEpicsState] =
    for {
      shutterState    <- sys.shutterState
      reqExposureTime <- sys.reqExposureTime
      ampReadMode     <- sys.ampReadMode
      gainSetting     <- sys.gainSetting
      ampCount        <- sys.ampCount
      roiNumUsed      <- sys.roiNumUsed
      ccdXBinning     <- sys.ccdXBinning
      ccdYBinning     <- sys.ccdYBinning
    } yield GmosDCEpicsState(shutterState, reqExposureTime, ampReadMode, gainSetting, ampCount, roiNumUsed, ccdXBinning, ccdYBinning)

  private def retrieveCCState: IO[GmosCCEpicsState] =
    for {
      filter1                <- sys.filter1
      filter2                <- sys.filter2
      disperserMode          <- sys.disperserMode
      disperser              <- sys.disperser
      disperserParked        <- sys.disperserParked
      disperserOrder         <- sys.disperserOrder
      disperserWavel         <- sys.disperserWavel
      fpu                    <- sys.fpu
      inBeam                 <- sys.inBeam
      stageMode              <- sys.stageMode
      dtaXOffset             <- sys.dtaXOffset
      dtaXCenter             <- sys.dtaXCenter
      useElectronicOffsetting <- sys.useElectronicOffsetting
    } yield GmosCCEpicsState(filter1, filter2, disperserMode, disperser, disperserParked, disperserOrder, disperserWavel, fpu, inBeam, stageMode, dtaXOffset, dtaXCenter, useElectronicOffsetting)

  private def setShutterState(s: GmosDCEpicsState, dc: DCConfig): Option[IO[Unit]] = dc.s match {
    case ShutterState.UnsetShutter => none
    case sh                        =>
      val encodedVal = encode(sh)
      applyParam(s.shutterState, encodedVal, DC.setShutterState)
  }

  private def setGainSetting[T<: GmosController.SiteDependentTypes: Encoders](state: GmosDCEpicsState, rm: AmpReadMode, g: AmpGain): Option[IO[Unit]] = {
    val encodedVal = Encoders[T].autoGain.encode((rm, g))
    applyParam(state.gainSetting, encodedVal, DC.setGainSetting)
  }

  private def roiNumUsed(s: RegionsOfInterest): Int =
    s.rois.map(_.length).getOrElse(1)

  private def setROI[T <: GmosController.SiteDependentTypes: Encoders](binning: CCDBinning, s: RegionsOfInterest): List[IO[Unit]] =
    s.rois match {
      case Left(b)     => roiParameters(binning, 1, Encoders[T].builtInROI.encode(b)).toList
      case Right(rois) => rois.zipWithIndex.flatMap { case (roi, i) =>
        roiParameters(binning, i + 1, ROIValues.fromOCS(roi))
      }
    }

  private def roiParameters(binning: CCDBinning, index: Int, roi: Option[ROIValues])
  : Option[IO[Unit]] = (roi, DC.rois.get(index)).mapN { (roi, r) =>
    r.setCcdXstart1(roi.xStart.value) *>
    r.setCcdXsize1(roi.xSize.value / binning.x.getValue) *>
    r.setCcdYstart1(roi.yStart.value) *>
    r.setCcdYsize1(roi.ySize.value / binning.y.getValue)
  }

  private def setFilters[T <: GmosController.SiteDependentTypes: Encoders](state: GmosCCEpicsState, f: T#Filter): List[Option[IO[Unit]]] = {
    val (filter1, filter2) = Encoders[T].filter.encode(f)

    List(
      applyParam(state.filter1, filter1, CC.setFilter1),
      applyParam(state.filter2, filter2, CC.setFilter2))
  }

  def setDisperser[T <: GmosController.SiteDependentTypes: Encoders](state: GmosCCEpicsState, d: T#Disperser): Option[IO[Unit]] = {
    val encodedVal = Encoders[T].disperser.encode(d)
    val s = applyParam(state.disperser.toUpperCase, encodedVal.toUpperCase, (_: String) => CC.setDisperser(encodedVal))
    // Force setting if disperser is parked
    s.orElse(
      applyParam(state.disperserParked, false, (_: Boolean) => CC.setDisperser(encodedVal))
    )
  }

  def setOrder(state: GmosCCEpicsState, o: DisperserOrder): Option[IO[Unit]] =
    applyParam(state.disperserOrder, disperserOrderEncoderInt.encode(o), (_: Int) => CC.setDisperserOrder(disperserOrderEncoder.encode(o)))

  def setDisperserParams[T <: GmosController.SiteDependentTypes: Encoders](state: GmosCCEpicsState, cfg: GmosController.Config[T], d: GmosController.Config[T]#GmosDisperser): List[Option[IO[Unit]]] = {
    val set: List[Option[IO[Unit]]] = d match {
      case cfg.GmosDisperser.Mirror =>
        val s: Option[IO[Unit]] = setDisperser(state, cfg.mirror)
        //If disperser is set, force mode configuration
        s.foldMap(_ => List(s, CC.setDisperserMode(disperserMode0).some))
      case cfg.GmosDisperser.Order0(d) =>
        val s0: Option[IO[Unit]] = setDisperser(state, d)
        // If disperser is set, force order configuration
        val s: List[Option[IO[Unit]]] = if(s0.isEmpty) List(setOrder(state, Order.ZERO))
                else CC.setDisperserOrder(disperserOrderEncoder.encode(Order.ZERO)).some :: List(s0)
        //If disperser or order are set, force mode configuration
        s.foldMap(_ => s ++ List(CC.setDisperserMode(disperserMode0).some))
      case cfg.GmosDisperser.OrderN(d, o, w) =>
        val s0: Option[IO[Unit]] = setDisperser(state, d)
        val s: List[Option[IO[Unit]]] =
          if (s0.isEmpty) {
            List(setOrder(state, o))
          } else {
            List(
              CC.setDisperserOrder(disperserOrderEncoder.encode(o)).some,
              s0,
              applyParam(state.disperserWavel, encode(w), CC.setDisperserLambda))
          }

        //If disperser, order or wavelength are set, force mode configuration
        s.foldMap(_ => s ++ List(CC.setDisperserMode(disperserMode0).some))
      //TODO Improve data model to remove this case. It is here because search includes types of
      // both sites.
      case _                   => List.empty
    }
    if (set.isEmpty) {
      List(applyParam(disperserModeDecode(state.disperserMode), disperserMode0, CC.setDisperserMode))
    } else {
      set
    }
  }

  def setFPU[T <: GmosController.SiteDependentTypes: Encoders](state: GmosCCEpicsState, cfg: GmosController.Config[T], cc: GmosFPU): List[Option[IO[Unit]]] = {
    def builtInFPU(fpu: T#FPU): (Option[String], Option[String]) = Encoders[T].fpu.encode(fpu)

    def customFPU(name: String): (Option[String], Option[String]) = name match {
      case "None" => (none, none)
      case _      => (name.some, beamEncoder.encode(Beam.InBeam).some)
    }

    def setFPUParams(p: (Option[String], Option[String])): List[Option[IO[Unit]]] = p match {
      case (fpuName, beam) =>
        List(fpuName.flatMap(v => applyParam(state.fpu, v, CC.setFpu)),
          beam.flatMap(v => applyParam(inBeamDecode(state.inBeam), v, CC.setInBeam)))
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

  private def setStage[T <: GmosController.SiteDependentTypes: Encoders](state: GmosCCEpicsState, v: T#GmosStageMode): Option[IO[Unit]] = {
    val stage = Encoders[T].stageMode.encode(v)

    applyParam(state.stageMode, stage, CC.setStageMode)
  }

  private def setDtaXOffset(state: GmosCCEpicsState, v: DTAX): Option[IO[Unit]] = {
    val PixelsToMicrons = 15.0
    val Tolerance = 0.001

    val offsetInMicrons =  v.intValue.toDouble * PixelsToMicrons

    // It seems that the reported dtaXOffset is absolute, but the applied offset is relative to
    // XCenter value
    applyParamT(Tolerance)(state.dtaXOffset - state.dtaXCenter, offsetInMicrons, CC.setDtaXOffset)
  }


  private def setElectronicOffset(state: GmosCCEpicsState, e: Option[UseElectronicOffset]): Option[IO[Unit]] = {
    val useEOffset = e.getOrElse(UseElectronicOffsettingRuling.deny(""))

    applyParam(state.useElectronicOffsetting, useEOffset.allow, (_: Boolean) => CC.setElectronicOffsetting(encode(useEOffset)))
  }

  val DhsConnected: String = "CONNECTED"

  def apply[T <: GmosController.SiteDependentTypes](cfg: GmosController.Config[T])(implicit e: Encoders[T]): GmosController[IO, T] =
    new GmosController[IO, T] {
      private def warnOnDHSNotConected: IO[Unit] =
        sys.dhsConnected.map(_.trim === DhsConnected).ifM(IO.unit,
          IO(Log.warn("GMOS is not connected to the DHS")))

      private def dcParams(state: GmosDCEpicsState, config: DCConfig): List[IO[Unit]] =
        List(
          applyParam(state.reqExposureTime, encode(config.t), (_: Int) => sys.configDCCmd.setExposureTime(config.t)),
          setShutterState(state, config),
          applyParam(state.ampReadMode, encode(config.r.ampReadMode), sys.configDCCmd.setAmpReadMode),
          setGainSetting(state, config.r.ampReadMode, config.r.ampGain),
          applyParam(state.ampCount, encode(config.r.ampCount), DC.setAmpCount),
          applyParam(state.roiNumUsed, roiNumUsed(config.roi), DC.setRoiNumUsed),
          applyParam(state.ccdXBinning, encode(config.bi.x), DC.setCcdXBinning),
          applyParam(state.ccdYBinning, encode(config.bi.y), DC.setCcdYBinning)
        ).flattenOption ++
          // TODO these are not smart about setting them only if needed
          setROI(config.bi, config.roi)

      private def nsParams(state: GmosNSEpicsState, config: NSConfig): List[IO[Unit]] =
        List(
          applyParam(state.nsPairs, config.nsPairs, (x: Int) => DC.setNsPairs(x))
        ).flattenOption

      private def ccParams(state: GmosCCEpicsState, config: Config[T]#CCConfig): List[IO[Unit]] =
        (setFilters(state, config.filter) ++
          setDisperserParams(state, cfg, config.disperser) ++
          setFPU(state, cfg, config.fpu) ++
          List(setStage(state, config.stage),
            setDtaXOffset(state, config.dtaX),
            setElectronicOffset(state, config.useElectronicOffset))).flattenOption

      override def applyConfig(config: GmosController.GmosConfig[T]): IO[Unit] = retrieveState.flatMap { state =>
        val params = dcParams(state.dc, config.dc) ++
                     ccParams(state.cc, config.cc) ++
                     nsParams(state.ns, config.ns)

        IO(Log.info("Start Gmos configuration")) *>
          IO(Log.debug(s"Gmos configuration: ${config.show}")) *>
          warnOnDHSNotConected *>
          (params.sequence *>
            sys.configCmd.setTimeout[IO](ConfigTimeout) *>
            sys.post
          ).unlessA(params.isEmpty) *>
          IO(Log.info("Completed Gmos configuration"))
      }

      override def observe(fileId: ImageFileId, expTime: Time): IO[ObserveCommandResult] =
        failOnDHSNotConected *>
          sys.observeCmd.setLabel(fileId) *>
          sys.observeCmd.setTimeout[IO](expTime + ReadoutTimeout) *>
          sys.observeCmd.post[IO]

      private def failOnDHSNotConected: IO[Unit] =
        sys.dhsConnected.map(_.trim === DhsConnected).ifM(IO.unit,
          IO.raiseError(SeqexecFailure.Execution("GMOS is not connected to DHS")))

      override def stopObserve: IO[Unit] =
        IO(Log.info("Stop Gmos exposure")) *>
          sys.stopCmd.setTimeout[IO](DefaultTimeout)
          sys.stopCmd.mark[IO] *>
          sys.stopCmd.post[IO]

      override def abortObserve: IO[Unit] =
        IO(Log.info("Abort Gmos exposure")) *>
          sys.abortCmd.setTimeout[IO](DefaultTimeout) *>
          sys.abortCmd.mark[IO] *>
          sys.abortCmd.post[IO].void

      override def endObserve: IO[Unit] =
        IO(Log.debug("Send endObserve to Gmos")) *>
          sys.endObserveCmd.setTimeout[IO](DefaultTimeout) *>
          sys.endObserveCmd.mark[IO] *>
          sys.endObserveCmd.post[IO].void

      override def pauseObserve: IO[Unit] =
        IO(Log.info("Send pause to Gmos")) *>
          sys.pauseCmd.setTimeout[IO](DefaultTimeout) *>
          sys.pauseCmd.mark[IO] *>
          sys.pauseCmd.post[IO].void

      override def resumePaused(expTime: Time): IO[ObserveCommandResult] = for {
        _   <- IO(Log.debug("Resume Gmos observation"))
        _   <- sys.continueCmd.setTimeout[IO](expTime + ReadoutTimeout)
        _   <- sys.continueCmd.mark[IO]
        ret <- sys.continueCmd.post[IO]
        _   <- IO(Log.debug("Completed Gmos observation"))
      } yield ret

      override def stopPaused: IO[ObserveCommandResult] = for {
        _   <- IO(Log.info("Stop Gmos paused observation"))
        _   <- sys.pauseCmd.setTimeout[IO](DefaultTimeout)
        _   <- sys.stopAndWaitCmd.mark[IO]
        ret <- sys.stopAndWaitCmd.post[IO]
        _   <- IO(Log.info("Completed stopping Gmos observation"))
      } yield if(ret === ObserveCommandResult.Success) ObserveCommandResult.Stopped else ret

      override def abortPaused: IO[ObserveCommandResult] = for {
        _   <- IO(Log.info("Abort Gmos paused observation"))
        _   <- sys.abortAndWait.setTimeout[IO](DefaultTimeout)
        _   <- sys.abortAndWait.mark[IO]
        ret <- sys.abortAndWait.post[IO]
        _   <- IO(Log.info("Completed aborting Gmos observation"))
      } yield if(ret === ObserveCommandResult.Success) ObserveCommandResult.Aborted else ret

      override def observeProgress(total: Time, elapsed: ElapsedTime): Stream[IO, Progress] = {
        implicit val ioTimer: Timer[IO] = IO.timer(ExecutionContext.global)
        EpicsUtil.countdown[IO](total, sys.countdown.map(_.seconds.some),
          IO(sys.observeState))
      }
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
    def fromInt(xStart: Int, xSize: Int, yStart: Int, ySize: Int): Option[ROIValues] =
      (XStart.fromInt(xStart), XSize.fromInt(xSize), YStart.fromInt(yStart), YSize.fromInt(ySize)).mapN(new ROIValues(_, _, _, _) {})

    // Built from OCS ROI values
    def fromOCS(roi: ROI): Option[ROIValues] =
      (XStart.fromInt(roi.getXStart), XSize.fromInt(roi.getXSize), YStart.fromInt(roi.getYStart), YSize.fromInt(roi.getYSize)).mapN(new ROIValues(_, _, _, _) {})

  }

  trait Encoders[T <: GmosController.SiteDependentTypes] {
    val filter: EncodeEpicsValue[T#Filter, (String, String)]
    val fpu: EncodeEpicsValue[T#FPU, (Option[String], Option[String])]
    val stageMode: EncodeEpicsValue[T#GmosStageMode, String]
    val disperser: EncodeEpicsValue[T#Disperser, String]
    val builtInROI: EncodeEpicsValue[BuiltinROI, Option[ROIValues]]
    val autoGain: EncodeEpicsValue[(AmpReadMode, AmpGain), Int]
  }

  object Encoders {
    @inline
    def apply[A <: GmosController.SiteDependentTypes](implicit ev: Encoders[A]): Encoders[A] = ev
  }

  val DefaultTimeout: Time = Seconds(60)
  val ReadoutTimeout: Time = Seconds(300)
  val ConfigTimeout: Time = Seconds(600)

}
