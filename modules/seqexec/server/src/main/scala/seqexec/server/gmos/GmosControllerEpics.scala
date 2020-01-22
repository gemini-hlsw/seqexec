// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.Applicative
import cats.ApplicativeError
import cats.effect.{Async, Timer}
import cats.implicits._
import edu.gemini.epics.acm.CarStateGeneric
import edu.gemini.spModel.gemini.gmos.GmosCommonType._
import io.chrisdavenport.log4cats.Logger
import fs2.Stream
import mouse.all._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.model.GmosParameters._
import seqexec.model.{NSSubexposure, ObserveStage}
import seqexec.model.enum.NodAndShuffleStage._
import seqexec.server.EpicsCodex.EncodeEpicsValue
import seqexec.server.EpicsUtil._
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.RemainingTime
import seqexec.server.NSProgress
import seqexec.server.{EpicsCommandBase, EpicsUtil, Progress, SeqexecFailure}
import seqexec.server.EpicsCodex._
import seqexec.server.gmos.GmosController.Config._
import seqexec.server.gmos.GmosController._
import squants.time.TimeConversions._
import squants.{Length, Time}
import shapeless.tag

import java.util.concurrent.TimeUnit.{SECONDS, MILLISECONDS}

import scala.concurrent.duration.FiniteDuration

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

  implicit val nsStateEncoder: EncodeEpicsValue[NodAndShuffleState, String] = EncodeEpicsValue{
    case NodAndShuffleState.Classic    => "CLASSIC"
    case NodAndShuffleState.NodShuffle => "NOD_SHUFFLE"
  }

  implicit val nsStateDecoder: DecodeEpicsValue[String, Option[NodAndShuffleState]] = DecodeEpicsValue{
    case "CLASSIC"     => NodAndShuffleState.Classic.some
    case "NOD_SHUFFLE" => NodAndShuffleState.NodShuffle.some
    case _             => none
  }

  implicit val exposureTimeEncoder: EncodeEpicsValue[ExposureTime, Int] = EncodeEpicsValue(_.toSeconds.toInt)

  implicit val disperserLambdaEncoder: EncodeEpicsValue[Length, Double] =
    EncodeEpicsValue((l: Length) => l.toNanometers)

  implicit val electronicOffsetEncoder: EncodeEpicsValue[ElectronicOffset, Int] =
    EncodeEpicsValue {
      case ElectronicOffset.On  => 1
      case ElectronicOffset.Off => 0
    }

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

object GmosEncoders extends GmosEncoders

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
  nsPairs: NsPairs,
  nsRows: NsRows,
  nsState: String
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
  useElectronicOffsetting: Int
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

  val DhsConnected: String = "CONNECTED"

  def apply[F[_]: Async, T <: GmosController.SiteDependentTypes](sys: => GmosEpics[F], cfg: GmosController.Config[T])(
    implicit e: Encoders[T],
             L: Logger[F],
             T: Timer[F]
  ): GmosController[F, T] =
    new GmosController[F, T] {
      private val CC = sys.configCmd
      private val DC = sys.configDCCmd

      // Read the current state of the
      private def retrieveState: F[GmosEpicsState] =
        for {
          dc <- retrieveDCState
          cc <- retrieveCCState
          ns <- retrieveNSState
        } yield GmosEpicsState(dc, cc, ns)

      private def retrieveNSState: F[GmosNSEpicsState] =
        (sys.nsPairs.map(tag[NsPairsI][Int]), sys.nsRows.map(tag[NsRowsI][Int]), sys.nsState)
          .mapN(GmosNSEpicsState.apply)

      private def retrieveDCState: F[GmosDCEpicsState] =
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

      private def retrieveCCState: F[GmosCCEpicsState] =
        for {
          filter1                 <- sys.filter1
          filter2                 <- sys.filter2
          disperserMode           <- sys.disperserMode
          disperser               <- sys.disperser
          disperserParked         <- sys.disperserParked
          disperserOrder          <- sys.disperserOrder
          disperserWavel          <- sys.disperserWavel
          fpu                     <- sys.fpu
          inBeam                  <- sys.inBeam
          stageMode               <- sys.stageMode
          dtaXOffset              <- sys.dtaXOffset
          dtaXCenter              <- sys.dtaXCenter
          useElectronicOffsetting <- sys.electronicOffset
        } yield GmosCCEpicsState(filter1, filter2, disperserMode, disperser, disperserParked, disperserOrder, disperserWavel, fpu, inBeam, stageMode, dtaXOffset, dtaXCenter, useElectronicOffsetting)

      private def setShutterState(s: GmosDCEpicsState, dc: DCConfig): Option[F[Unit]] = dc.s match {
        case ShutterState.UnsetShutter => none
        case sh                        =>
          val encodedVal = encode(sh)
          applyParam(s.shutterState, encodedVal, DC.setShutterState)
      }

      private def setGainSetting(state: GmosDCEpicsState, rm: AmpReadMode, g: AmpGain): Option[F[Unit]] = {
        val encodedVal = Encoders[T].autoGain.encode((rm, g))
        applyParam(state.gainSetting, encodedVal, DC.setGainSetting)
      }

      private def roiNumUsed(s: RegionsOfInterest): Int =
        s.rois.map(_.length).getOrElse(1)

      private def setROI(binning: CCDBinning, s: RegionsOfInterest): List[F[Unit]] =
        s.rois match {
          case Left(b)     => roiParameters(binning, 1, Encoders[T].builtInROI.encode(b)).toList
          case Right(rois) => rois.zipWithIndex.flatMap { case (roi, i) =>
            roiParameters(binning, i + 1, ROIValues.fromOCS(roi))
          }
        }

      private def roiParameters(binning: CCDBinning, index: Int, roi: Option[ROIValues])
      : Option[F[Unit]] = (roi, DC.rois.get(index)).mapN { (roi, r) =>
        r.setCcdXstart1(roi.xStart.value) *>
        r.setCcdXsize1(roi.xSize.value / binning.x.getValue) *>
        r.setCcdYstart1(roi.yStart.value) *>
        r.setCcdYsize1(roi.ySize.value / binning.y.getValue)
      }

      private def setFilters(state: GmosCCEpicsState, f: T#Filter): List[Option[F[Unit]]] = {
        val (filter1, filter2) = Encoders[T].filter.encode(f)

        List(
          applyParam(state.filter1, filter1, CC.setFilter1),
          applyParam(state.filter2, filter2, CC.setFilter2))
      }

      def setDisperser(state: GmosCCEpicsState, d: T#Disperser): Option[F[Unit]] = {
        val encodedVal = Encoders[T].disperser.encode(d)
        val s = applyParam(state.disperser.toUpperCase, encodedVal.toUpperCase, (_: String) => CC.setDisperser(encodedVal))
        // Force setting if not set and disperser is parked
        s.orElse(state.disperserParked.option(CC.setDisperser(encodedVal)))
      }

      def setOrder(state: GmosCCEpicsState, o: DisperserOrder): Option[F[Unit]] =
        applyParam(state.disperserOrder, disperserOrderEncoderInt.encode(o), (_: Int) => CC.setDisperserOrder(disperserOrderEncoder.encode(o)))

      def setWavelength(state: GmosCCEpicsState, w: Length): Option[F[Unit]] =
        applyParam(state.disperserWavel, encode(w), CC.setDisperserLambda)

      def setDisperserParams(
        state: GmosCCEpicsState,
        cfg: GmosController.Config[T],
        d: GmosController.Config[T]#GmosDisperser
      ): List[Option[F[Unit]]] = {
        val params: List[Option[F[Unit]]] = d match {
          case cfg.GmosDisperser.Mirror => List(setDisperser(state, cfg.mirror))
          case cfg.GmosDisperser.Order0(d) =>
            val s0: Option[F[Unit]] = setDisperser(state, d)
            // If disperser is set, force order configuration
            if(s0.isEmpty) List(setOrder(state, Order.ZERO))
            else CC.setDisperserOrder(disperserOrderEncoder.encode(Order.ZERO)).some :: List(s0)
          case cfg.GmosDisperser.OrderN(d, o, w) =>
            val s0: Option[F[Unit]] = setDisperser(state, d) // None means disperser is already in the right position
            // If disperser is set, the order has to be set regardless of current value.
            if (s0.isEmpty) {
              List(
                setOrder(state, o),
                setWavelength(state, w)
              )
            } else {
              List(
                CC.setDisperserOrder(disperserOrderEncoder.encode(o)).some,
                s0,
                setWavelength(state, w)
              )
            }

          //TODO Improve data model to remove this case. It is here because search includes types of
          // both sites.
          case _                   => List.empty
        }

        //If disperser, order or wavelength are set, force mode configuration. If not, check if it needs to be set anyways
        if(params.exists(_.isDefined)) params :+ CC.setDisperserMode(disperserMode0).some
        else List(applyParam(disperserModeDecode(state.disperserMode), disperserMode0, CC.setDisperserMode))
      }

      def setFPU(state: GmosCCEpicsState, cfg: GmosController.Config[T], cc: GmosFPU): List[Option[F[Unit]]] = {
        def builtInFPU(fpu: T#FPU): (Option[String], Option[String]) = Encoders[T].fpu.encode(fpu)

        def customFPU(name: String): (Option[String], Option[String]) = name match {
          case "None" => (none, beamEncoder.encode(Beam.OutOfBeam).some)
          case _      => (name.some, beamEncoder.encode(Beam.InBeam).some)
        }

        def setFPUParams(p: (Option[String], Option[String])): List[Option[F[Unit]]] = p match {
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

      private def setStage(state: GmosCCEpicsState, v: T#GmosStageMode): Option[F[Unit]] = {
        val stage = Encoders[T].stageMode.encode(v)

        applyParam(state.stageMode, stage, CC.setStageMode)
      }

      private def setDtaXOffset(state: GmosCCEpicsState, v: DTAX): Option[F[Unit]] = {
        val PixelsToMicrons = 15.0
        val Tolerance = 0.001

        val offsetInMicrons =  v.intValue.toDouble * PixelsToMicrons

        // It seems that the reported dtaXOffset is absolute, but the applied offset is relative to
        // XCenter value
        applyParamT(Tolerance)(state.dtaXOffset - state.dtaXCenter, offsetInMicrons, CC.setDtaXOffset)
      }

      private def setElectronicOffset(state: GmosCCEpicsState, e: ElectronicOffset): Option[F[Unit]] =
        applyParam(state.useElectronicOffsetting, encode(e), (e: Int) => CC.setElectronicOffsetting(e))

      private def warnOnDHSNotConected: F[Unit] =
        sys.dhsConnected.map(_.trim === DhsConnected).ifM(Applicative[F].unit,
          L.warn("GMOS is not connected to the DHS"))

      private def dcParams(state: GmosDCEpicsState, config: DCConfig): List[F[Unit]] =
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

      private def nsParams(state: GmosNSEpicsState, config: NSConfig): List[F[Unit]] =
        List(
          applyParam(state.nsPairs, config.nsPairs, (x: Int) => DC.setNsPairs(x)),
          applyParam(state.nsRows, config.nsRows, (x: Int) => DC.setNsRows(x)),
          applyParam(state.nsState, encode(config.nsState), DC.setNsState)
        ).flattenOption

      // Don't set CC if Dark or Bias
      private def ccParams(state: GmosCCEpicsState, config: Config[T]#CCConfig): List[F[Unit]] =
        if(config.isDarkOrBias) List.empty
        else (
          setFilters(state, config.filter) ++
          setDisperserParams(state, cfg, config.disperser) ++
          setFPU(state, cfg, config.fpu) ++
          List(
            setStage(state, config.stage),
            setDtaXOffset(state, config.dtaX),
            setElectronicOffset(state, config.useElectronicOffset)
          )
        ).flattenOption

      override def applyConfig(config: GmosController.GmosConfig[T]): F[Unit] = retrieveState.flatMap { state =>
        val params = dcParams(state.dc, config.dc) ++
                     ccParams(state.cc, config.cc) ++
                     nsParams(state.ns, config.ns)

        L.debug("Start Gmos configuration") *>
          L.debug(s"Gmos configuration: ${config.show}") *>
          warnOnDHSNotConected *>
          (params.sequence *>
            sys.post(ConfigTimeout)
          ).unlessA(params.isEmpty) *>
          L.debug("Completed Gmos configuration")
      }

      override def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] =
        failOnDHSNotConected *>
          sys.observeCmd.setLabel(fileId) *>
          sys.observeCmd.post(FiniteDuration(expTime.toMillis, MILLISECONDS) + ReadoutTimeout)

      private def failOnDHSNotConected: F[Unit] =
        sys.dhsConnected.map(_.trim === DhsConnected).ifM(Applicative[F].unit,
          ApplicativeError[F, Throwable].raiseError(SeqexecFailure.Execution("GMOS is not connected to DHS")))

      private def protectedObserveCommand(name: String, cmd: EpicsCommandBase[F]): F[Unit] = {
        val safetyCutoffAsDouble: Double = SafetyCutoff.toSeconds.toDouble

        (sys.dcIsAcquiring, sys.countdown).mapN { case (isAcq, timeLeft) =>
          if(!isAcq)
            L.debug(s"Gmos $name Observe canceled because it is not acquiring.")
          else if(timeLeft <= safetyCutoffAsDouble)
            L.debug(s"Gmos $name Observe canceled because there is less than $safetyCutoffAsDouble seconds left.")
          else
            L.debug(s"$name Gmos exposure") *>
              cmd.mark *>
              cmd.post(DefaultTimeout).void
        }.flatten
      }

      override def stopObserve: F[Unit] = protectedObserveCommand("Stop", sys.stopCmd)

      override def abortObserve: F[Unit] = protectedObserveCommand("Abort", sys.abortCmd)

      override def endObserve: F[Unit] =
        L.debug("Send endObserve to Gmos") *>
          sys.endObserveCmd.mark *>
          sys.endObserveCmd.post(DefaultTimeout).void

      override def pauseObserve: F[Unit] = protectedObserveCommand("Pause", sys.pauseCmd)

      override def resumePaused(expTime: Time): F[ObserveCommandResult] = for {
        _   <- L.debug("Resume Gmos observation")
        _   <- sys.continueCmd.mark
        ret <- sys.continueCmd.post(FiniteDuration(expTime.toMillis, MILLISECONDS) + ReadoutTimeout)
        _   <- L.debug("Completed Gmos observation")
      } yield ret

      override def stopPaused: F[ObserveCommandResult] = for {
        _   <- L.debug("Stop Gmos paused observation")
        _   <- sys.stopAndWaitCmd.mark
        ret <- sys.stopAndWaitCmd.post(DefaultTimeout)
        _   <- L.debug("Completed stopping Gmos observation")
      } yield if(ret === ObserveCommandResult.Success) ObserveCommandResult.Stopped else ret

      override def abortPaused: F[ObserveCommandResult] = for {
        _   <- L.debug("Abort Gmos paused observation")
        _   <- sys.abortAndWait.mark
        ret <- sys.abortAndWait.post(DefaultTimeout)
        _   <- L.debug("Completed aborting Gmos observation")
      } yield if(ret === ObserveCommandResult.Success) ObserveCommandResult.Aborted else ret

      // Calculate the current subexposure
      def nsSubExposure: F[NSSubexposure] =
        (sys.nsPairs, sys.currentCycle, sys.aExpCount, sys.bExpCount).mapN { (total, cycle, aCount, bCount) =>
          val stageIndex = (aCount + bCount) % NsSequence.length
          val sub = NSSubexposure(
            tag[NsCyclesI][Int](total / 2),
            tag[NsCyclesI][Int](cycle),
            stageIndex)
          sub.getOrElse(NSSubexposure.Zero)
        }

      // Different progress results for classic and NS
      def gmosProgress: (Time, RemainingTime, ObserveStage) => F[Progress] =
        (time, remaining, stage) => sys.nsState.flatMap {
          case "CLASSIC" => EpicsUtil.defaultProgress[F](time, remaining, stage)
          case _         => nsSubExposure.map(s => NSProgress(time, remaining, stage, s))
        }

      override def observeProgress(total: Time, elapsed: ElapsedTime): Stream[F, Progress] =
        EpicsUtil.countdown[F](
          total, sys.countdown.map(_.seconds),
          sys.observeState.widen[CarStateGeneric],
          (sys.dcIsPreparing, sys.dcIsAcquiring, sys.dcIsReadingOut).mapN(ObserveStage.fromBooleans),
          gmosProgress
        )

      override def nsCount: F[Int] = for{
        a <- sys.aExpCount
        b <- sys.bExpCount
      } yield a + b
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

  val DefaultTimeout: FiniteDuration = FiniteDuration(60, SECONDS)
  val ReadoutTimeout: FiniteDuration = FiniteDuration(90, SECONDS)
  val ConfigTimeout: FiniteDuration = FiniteDuration(600, SECONDS)

}
