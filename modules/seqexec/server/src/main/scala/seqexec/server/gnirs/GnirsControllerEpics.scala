// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.implicits._
import cats.effect.{Async, Sync, Timer}
import seqexec.server._
import edu.gemini.spModel.gemini.gnirs.GNIRSParams
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.{Camera, Decker, Disperser, ReadMode}
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.model.ObserveStage
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.EpicsUtil._
import seqexec.server.EpicsCodex._
import seqexec.server.gnirs.GnirsController._
import squants.electro.Millivolts
import squants.space.LengthConversions._
import squants.time.TimeConversions._
import squants.{Length, Time}

import java.util.concurrent.TimeUnit.{SECONDS, MILLISECONDS}

import scala.concurrent.duration.FiniteDuration

trait GnirsEncoders {
  val readModeEncoder: EncodeEpicsValue[ReadMode, (Int, Int)] = EncodeEpicsValue {
    case ReadMode.VERY_BRIGHT => (1, 1)
    case ReadMode.BRIGHT      => (1, 16)
    case ReadMode.FAINT       => (16, 16)
    case ReadMode.VERY_FAINT  => (32, 16)
  }

  implicit val wellDepthEncoder: EncodeEpicsValue[GNIRSParams.WellDepth, Double] = EncodeEpicsValue {
    w => Millivolts(w.getBias).toVolts
  }

  implicit val cameraDecoder: EncodeEpicsValue[Camera, String] = EncodeEpicsValue {
    case Camera.SHORT_RED  => "ShortRed"
    case Camera.SHORT_BLUE => "ShortBlue"
    case Camera.LONG_RED   => "LongRed"
    case Camera.LONG_BLUE  => "LongBlue"
  }

  implicit val slitWidthEncoder: EncodeEpicsValue[SlitWidth, String] = EncodeEpicsValue {
    case SlitWidth.Slit0_10     => "0.10arcsec"
    case SlitWidth.Slit0_15     => "0.15arcsec"
    case SlitWidth.Slit0_20     => "0.20arcsec"
    case SlitWidth.Slit0_30     => "0.30arcsec"
    case SlitWidth.Slit0_45     => "0.45arcsec"
    case SlitWidth.Slit0_68     => "0.68arcsec"
    case SlitWidth.Slit1_00     => "1.0arcsec"
    case SlitWidth.PupilViewer  => "PV"
    case SlitWidth.SmallPinhole => "SmPinholes"
    case SlitWidth.LargePinhole => "LgPinholes"
    case SlitWidth.Acquisition  => "Acq"
  }

  implicit val deckerEncoder: EncodeEpicsValue[Decker, String] = EncodeEpicsValue {
    case Decker.ACQUISITION         => "Acq"
    case Decker.PUPIL_VIEWER        => "PV"
    case Decker.SHORT_CAM_LONG_SLIT => "SCLong"
    case Decker.SHORT_CAM_X_DISP    => "SCXD"
    case Decker.LONG_CAM_LONG_SLIT  => "LCLong"
    case Decker.LONG_CAM_X_DISP     => "LCXD"
    case Decker.IFU                 => "IFU1"
    case Decker.WOLLASTON           => "Woll"
  }

  implicit val filter1Encoder: EncodeEpicsValue[Filter1, String] = EncodeEpicsValue {
    case Filter1.Open        => "Open"
    case Filter1.J_MK        => "J_MK"
    case Filter1.K_MK        => "K_MK"
    case Filter1.Y_MK        => "Y_MK"
    case Filter1.ND100X      => "ND100X"
    case Filter1.PupilViewer => "PupilViewer"
    case Filter1.RightMask   => "RightMask"
    case Filter1.LeftMask    => "LeftMask"
  }

  implicit val filter2Encoder: EncodeEpicsValue[Filter2Pos, String] = EncodeEpicsValue {
    case Filter2Pos.H    => "H"
    case Filter2Pos.H2   => "H2"
    case Filter2Pos.J    => "J"
    case Filter2Pos.K    => "K"
    case Filter2Pos.L    => "L"
    case Filter2Pos.M    => "M"
    case Filter2Pos.Open => "Open"
    case Filter2Pos.PAH  => "PAH"
    case Filter2Pos.X    => "X"
    case Filter2Pos.XD   => "XD"
  }

  implicit val wavelEncoder: EncodeEpicsValue[Wavelength, Double] = EncodeEpicsValue(_.toNanometers)
}

object GnirsControllerEpics extends GnirsEncoders {

  private def autoFilter(wavel: Length): GnirsController.Filter2Pos = {
    val table = List(
      GnirsController.Filter2Pos.X -> 1.17,
      GnirsController.Filter2Pos.J -> 1.42,
      GnirsController.Filter2Pos.H -> 1.86,
      GnirsController.Filter2Pos.K -> 2.70,
      GnirsController.Filter2Pos.L -> 4.30,
      GnirsController.Filter2Pos.M -> 6.0
    ).map{ case (f, w) => (f, w.nanometers) }

    table.foldRight[GnirsController.Filter2Pos](GnirsController.Filter2Pos.XD){
      case (t, v) => if(wavel < t._2) t._1 else v
    }
  }

  def apply[F[_]: Async: Timer](epicsSys: => GnirsEpics[F])(implicit L: Logger[F]): GnirsController[F] =
    new GnirsController[F] {

      private val ccCmd = epicsSys.configCCCmd
      private val dcCmd = epicsSys.configDCCmd

      private val warnOnDhs = epicsSys.dhsConnected.flatMap(L.warn("GNIRS is not connected to DHS").unlessA)

      private val warnOnArray =
        epicsSys.arrayActive.flatMap(L.warn("GNIRS detector array is not active").unlessA)

      private val checkDhs = failUnlessM(epicsSys.dhsConnected, SeqexecFailure.Execution("GNIRS is not connected to DHS"))

      private val checkArray =
        failUnlessM(epicsSys.arrayActive, SeqexecFailure.Execution("GNIRS detector array is not active"))

      private def setAcquisitionMirror(mode: Mode): F[Option[F[Unit]]] = {
        val v = mode match {
          case Acquisition => "In"
          case _           => "Out"
        }

        smartSetParamF(v, epicsSys.acqMirror.map(removePartName), ccCmd.setAcqMirror(v))
      }

      private def setGrating(s: Spectrography, c: Camera): List[F[Option[F[Unit]]]] = {
        def stdConversion(d: Disperser): String = (d, c) match {
          case (Disperser.D_10, Camera.SHORT_RED)   => "10/mmSR"
          case (Disperser.D_32, Camera.SHORT_RED)   => "32/mmSR"
          case (Disperser.D_111, Camera.SHORT_RED)  => "111/mmSR"
          case (Disperser.D_10, Camera.LONG_RED)    => "10/mmLR"
          case (Disperser.D_32, Camera.LONG_RED)    => "32/mmLR"
          case (Disperser.D_111, Camera.LONG_RED)   => "111/mmLR"
          case (Disperser.D_10, Camera.SHORT_BLUE)  => "10/mmSB"
          case (Disperser.D_32, Camera.SHORT_BLUE)  => "32/mmSB"
          case (Disperser.D_111, Camera.SHORT_BLUE) => "111/mmSB"
          case (Disperser.D_10, Camera.LONG_BLUE)   => "10/mmLB"
          case (Disperser.D_32, Camera.LONG_BLUE)   => "32/mmLB"
          case (Disperser.D_111, Camera.LONG_BLUE)  => "111/mmLB"
        }

        val v = s match {
          case CrossDisperserS(Disperser.D_10) if c === Camera.LONG_BLUE => "10/mmLBSX"
          case CrossDisperserL(Disperser.D_10) if c === Camera.LONG_BLUE => "10/mmLBLX"
          case _                                                         => stdConversion(s.disperser)
        }

        val defaultMode = "WAVELENGTH"

        List(
          smartSetParamF(v, epicsSys.grating.map(removePartName), ccCmd.setGrating(v)),
          smartSetParamF(defaultMode, epicsSys.gratingMode, ccCmd.setGratingMode(defaultMode)))
      }

      private def setSpectrographyComponents(mode: Mode, c: Camera): List[F[Option[F[Unit]]]] = mode match {
        case Acquisition => Nil
        case s:Spectrography => setGrating(s, c) :+ setPrism(s, c)
      }

      private def setPrism(s: Spectrography, c: Camera): F[Option[F[Unit]]] = {
        val cameraStr = c match {
          case Camera.LONG_BLUE  => "LB"
          case Camera.LONG_RED   => "LR"
          case Camera.SHORT_BLUE => "SB"
          case Camera.SHORT_RED  => "SR"
        }

        val v = s match {
          case Mirror(_)          => "MIR"
          case Wollaston(_)       => "WOLL"
          case CrossDisperserL(_) => s"$cameraStr+LXD"
          case CrossDisperserS(_) => s"$cameraStr+SXD"
        }

        smartSetParamF(v, epicsSys.prism.map(removePartName), ccCmd.setPrism(v))
      }

      private def setDarkCCParams: List[F[Option[F[Unit]]]] = {
        val closed = "Closed"
        val darkFilter = "Dark"
        List(
          smartSetParamF(closed, epicsSys.cover.map(removePartName), ccCmd.setCover(closed)),
          smartSetParamF(darkFilter, epicsSys.filter1.map(removePartName), ccCmd.setFilter1(darkFilter)))
      }

      private def setFilter2(f: Filter2, w: Wavelength): F[Option[F[Unit]]] = {
        val pos = f match {
          case Manual(p) => p
          case Auto      => autoFilter(w)
        }
        smartSetParamF(encode(pos), epicsSys.filter2.map(removePartName), ccCmd.setFilter2(encode(pos)))
      }

      private def setOtherCCParams(config: Other): List[F[Option[F[Unit]]]] = {
        val open = "Open"
        val bestFocus = "best focus"
        val wavelengthTolerance = 0.0001
        val filter1 = smartSetParamF(encode(config.filter1), epicsSys.filter1.map(removePartName), ccCmd.setFilter1(encode(config.filter1)))
        val filter2 = setFilter2(config.filter2, config.wavel)
        val camera = smartSetParamF(encode(config.camera), epicsSys.camera.map(removePartName), ccCmd.setCamera(encode(config.camera)))
        val spectrographyAndCamera = setSpectrographyComponents(config.mode, config.camera) :+ camera
        val params: List[F[Option[F[Unit]]]] = List(
          setAcquisitionMirror(config.mode),
          filter1,
          filter2) ::: spectrographyAndCamera

        val focusParam: F[Option[F[Unit]]] = config.focus match {
          case Focus.Best      => Sync[F].delay(ccCmd.setFocusBest(bestFocus).some)
          case Focus.Manual(v) => smartSetParamF(v, epicsSys.focusEng, ccCmd.setFocus(v))
        }

        val cover = smartSetParamF(open, epicsSys.cover.map(removePartName), ccCmd.setCover(open))
        val slitWidth  = config.slitWidth.map(sl => smartSetParamF(encode(sl), epicsSys.slitWidth.map(removePartName), ccCmd.setSlitWidth(encode(sl)))).getOrElse(none.pure[F])
        val decker = smartSetParamF(encode(config.decker), epicsSys.decker.map(removePartName), ccCmd.setDecker(encode(config.decker)))
        val centralWavelength = smartSetDoubleParamF(wavelengthTolerance)(encode(config.wavel), epicsSys.centralWavelength, ccCmd.setCentralWavelength(encode(config.wavel)))

        (cover :: params) :::
          List(focusParam, slitWidth, decker, centralWavelength)

      }

      private def setCCParams(config: CCConfig): F[Unit] = {
        val params = config match {
          case Dark    => setDarkCCParams
          case c: Other => setOtherCCParams(c)
        }
        executeIfNeeded(
          params,
          ccCmd.post(ConfigTimeout)
        )
      }

      private def setDCParams(config: DCConfig): F[Unit] = {

        val expTimeTolerance = 0.0001
        // Old Seqexec has an absolute tolerance of 0.05V, which is 16.7% relative tolerance for
        // 0.3V bias
        val biasTolerance = 0.15

        val (lowNoise, digitalAvgs) = readModeEncoder.encode(config.readMode)

        val expTimeWriter = smartSetDoubleParamF(expTimeTolerance)(config.exposureTime.toSeconds,
          epicsSys.exposureTime, dcCmd.setExposureTime(config.exposureTime.toSeconds))

        val coaddsWriter = smartSetParamF(config.coadds, epicsSys.numCoadds,
          dcCmd.setCoadds(config.coadds))

        // Value read from the instrument is the negative of what was set
        val biasWriter = smartSetDoubleParamF(biasTolerance)(-encode(config.wellDepth), epicsSys.detBias,
          dcCmd.setDetBias(encode(config.wellDepth)))

        val lowNoiseWriter = smartSetParamF(lowNoise, epicsSys.lowNoise, dcCmd.setLowNoise(lowNoise))

        val digitalAvgsWriter = smartSetParamF(digitalAvgs, epicsSys.digitalAvgs,
          dcCmd.setDigitalAvgs(digitalAvgs))

        val params = List(expTimeWriter, coaddsWriter, biasWriter, lowNoiseWriter, digitalAvgsWriter)

        executeIfNeeded(
          params,
          dcCmd.post(DefaultTimeout)
        )
      }

      override def applyConfig(config: GnirsConfig): F[Unit] =
        L.debug("Starting GNIRS configuration") *>
          warnOnDhs *>
          warnOnArray *>
          setDCParams(config.dc) *>
          setCCParams(config.cc) *>
          L.debug("Completed GNIRS configuration")

      override def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] =
        L.debug(s"Start GNIRS observe, file id $fileId") *>
          checkDhs *>
          checkArray *>
          epicsSys.observeCmd.setLabel(fileId) *>
          epicsSys.observeCmd.post(FiniteDuration(expTime.toMillis, MILLISECONDS) + ReadoutTimeout).flatTap{ _ => L.debug("Completed GNITS observe") }

      override def endObserve: F[Unit] =
        L.debug("Send endObserve to GNIRS") *>
          epicsSys.endObserveCmd.mark *>
          epicsSys.endObserveCmd.post(DefaultTimeout).void

      override def stopObserve: F[Unit] =
        L.debug("Stop GNIRS exposure") *>
          epicsSys.stopCmd.mark *>
          epicsSys.stopCmd.post(DefaultTimeout).void

      override def abortObserve: F[Unit] =
        L.debug("Abort GNIRS exposure") *>
          epicsSys.abortCmd.mark *>
          epicsSys.abortCmd.post(DefaultTimeout).void

      override def observeProgress(total: Time): Stream[F, Progress] =
        ProgressUtil.obsCountdownWithObsStage[F](total, 0.seconds,
          (epicsSys.dcIsPreparing, epicsSys.dcIsAcquiring, epicsSys.dcIsReadingOut).mapN(ObserveStage.fromBooleans)
        )

      override def calcTotalExposureTime(cfg: GnirsController.DCConfig): F[Time] =
        GnirsController.calcTotalExposureTime[F](cfg)
    }

  private val DefaultTimeout: FiniteDuration = FiniteDuration(60, SECONDS)
  private val ReadoutTimeout: FiniteDuration = FiniteDuration(30, SECONDS)
  private val ConfigTimeout: FiniteDuration = FiniteDuration(240, SECONDS)
}
