// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.implicits._
import cats.effect.{ IO, Timer }
import cats.data.Nested
import seqexec.model.dhs.ImageFileId
import seqexec.server._
import edu.gemini.spModel.gemini.gnirs.GNIRSParams
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.{Camera, Decker, Disperser, ReadMode}
import fs2.Stream
import org.log4s.getLogger
import squants.{Length, Seconds, Time}
import squants.space.LengthConversions._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.EpicsUtil._
import seqexec.server.EpicsCodex._
import seqexec.server.gnirs.GnirsController._
import squants.electro.Millivolts
import squants.space.LengthConversions._
import squants.time.TimeConversions._
import squants.{Length, Seconds, Time}

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
  private val Log = getLogger

  private def epicsSys = GnirsEpics.instance
  private def ccCmd = epicsSys.configCCCmd
  private def dcCmd = epicsSys.configDCCmd

  private def setAcquisitionMirror(mode: Mode): IO[Option[IO[Unit]]] = {
    val v = mode match {
      case Acquisition => "In"
      case _           => "Out"
    }

    smartSetParamF(v, Nested(epicsSys.acqMirror).map(removePartName).value, ccCmd.setAcqMirror(v))
  }

  private def setGrating(s: Spectrography, c: Camera): List[IO[Option[IO[Unit]]]] = {
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
      smartSetParamF(v, Nested(epicsSys.grating).map(removePartName).value, ccCmd.setGrating(v)),
      smartSetParamF(defaultMode, epicsSys.gratingMode, ccCmd.setGratingMode(defaultMode)))
  }

  private def setPrism(s: Spectrography, c: Camera): IO[Option[IO[Unit]]] = {
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

    smartSetParamF(v, Nested(epicsSys.prism).map(removePartName).value, ccCmd.setPrism(v))
  }

  private def setDarkCCParams: List[IO[Option[IO[Unit]]]] = {
    val closed = "Closed"
    val darkFilter = "Dark"
    List(
      smartSetParamF(closed, Nested(epicsSys.cover).map(removePartName).value, ccCmd.setCover(closed)),
      smartSetParamF(darkFilter, Nested(epicsSys.filter1).map(removePartName).value, ccCmd.setFilter1(darkFilter)))
  }

  private def setSpectrographyComponents(mode: Mode, c: Camera): List[IO[Option[IO[Unit]]]] = mode match {
    case Acquisition => Nil
    case s:Spectrography => setGrating(s, c) :+ setPrism(s, c)
  }

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

  private def setFilter2(f: Filter2, w: Wavelength): IO[Option[IO[Unit]]] = {
    val pos = f match {
      case Manual(p) => p
      case Auto      => autoFilter(w)
    }
    smartSetParamF(encode(pos), Nested(epicsSys.filter2).map(removePartName).value, ccCmd.setFilter2(encode(pos)))
  }

  private def setOtherCCParams(config: Other): List[IO[Option[IO[Unit]]]] = {
    val open = "Open"
    val bestFocus = "best focus"
    val wavelengthTolerance = 0.0001
    val filter1 = smartSetParamF(encode(config.filter1), Nested(epicsSys.filter1).map(removePartName).value, ccCmd.setFilter1(encode(config.filter1)))
    val filter2 = setFilter2(config.filter2, config.wavel)
    val camera = smartSetParamF(encode(config.camera), Nested(epicsSys.camera).map(removePartName).value, ccCmd.setCamera(encode(config.camera)))
    val spectrographyAndCamera = setSpectrographyComponents(config.mode, config.camera) :+ camera
    val params: List[IO[Option[IO[Unit]]]] = List(
      setAcquisitionMirror(config.mode),
      filter1,
      filter2) ::: spectrographyAndCamera

    val focusParam: IO[Option[IO[Unit]]] = config.focus match {
      case Focus.Best => IO(ccCmd.setFocusBest(bestFocus).some)
      case Focus.Manual(v) => smartSetParamF(v, epicsSys.focusEng, ccCmd.setFocus(v))
    }

    val cover = smartSetParamF(open, Nested(epicsSys.cover).map(removePartName).value, ccCmd.setCover(open))
    val slitWidth  = config.slitWidth.map(sl => smartSetParamF(encode(sl), Nested(epicsSys.slitWidth).map(removePartName).value, ccCmd.setSlitWidth(encode(sl)))).orEmpty
    val decker = smartSetParamF(encode(config.decker), Nested(epicsSys.decker).map(removePartName).value, ccCmd.setDecker(encode(config.decker)))
    val centralWavelength = smartSetDoubleParamF(wavelengthTolerance)(encode(config.wavel), epicsSys.centralWavelength, ccCmd.setCentralWavelength(encode(config.wavel)))

    (cover :: params) :::
      List(focusParam, slitWidth, decker, centralWavelength)

  }

  private def setCCParams(config: CCConfig): IO[Unit] = {
    val params = config match {
      case Dark    => setDarkCCParams
      case c: Other => setOtherCCParams(c)
    }
    executeIfNeeded(
      params,
      ccCmd.setTimeout[IO](ConfigTimeout) *>
      ccCmd.post[IO])
  }

  private def setDCParams(config: DCConfig): IO[Unit] = {

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

    executeIfNeeded(params,
      dcCmd.setTimeout[IO](DefaultTimeout) *>
      dcCmd.post[IO])
  }

  val checkDhs =
    failUnlessM(
      epicsSys.dhsConnected.map(_.contains(true)),
                SeqexecFailure.Execution("GNIRS is not connected to DHS"))

  val checkArray =
    failUnlessM(
      epicsSys.arrayActive.map(_.contains(true)),
                SeqexecFailure.Execution("GNIRS detector array is not active"))

  val warnOnDhs =
    epicsSys.dhsConnected.map(_.contains(true)).flatMap(
       IO(Log.warn("GNIRS is not connected to DHS")).unlessA
    )

  val warnOnArray =
    epicsSys.arrayActive.map(_.contains(true)).flatMap(
       IO(Log.warn("GNIRS detector array is not active")).unlessA
    )

  def apply()(implicit ioTimer: Timer[IO]): GnirsController[IO] =
    new GnirsController[IO] {

      override def applyConfig(config: GnirsConfig): IO[Unit] =
        IO(Log.debug("Starting GNIRS configuration")) *>
          warnOnDhs *>
          warnOnArray *>
          setDCParams(config.dc) *>
          setCCParams(config.cc) *>
          IO(Log.debug("Completed GNIRS configuration"))

      override def observe(fileId: ImageFileId, expTime: Time): IO[ObserveCommandResult] =
        IO(Log.info("Start GNIRS observe")) *>
          checkDhs *>
          checkArray *>
          epicsSys.observeCmd.setLabel(fileId) *>
          epicsSys.observeCmd.setTimeout[IO](expTime + ReadoutTimeout) *>
          epicsSys.observeCmd.post[IO]

      override def endObserve: IO[Unit] =
        IO(Log.debug("Send endObserve to GNIRS")) *>
          epicsSys.endObserveCmd.setTimeout[IO](DefaultTimeout) *>
          epicsSys.endObserveCmd.mark[IO] *>
          epicsSys.endObserveCmd.post[IO].void

      override def stopObserve: IO[Unit] =
        IO(Log.info("Stop GNIRS exposure")) *>
          epicsSys.stopCmd.setTimeout[IO](DefaultTimeout) *>
          epicsSys.stopCmd.mark[IO] *>
          epicsSys.stopCmd.post[IO].void

      override def abortObserve: IO[Unit] =
        IO(Log.info("Abort GNIRS exposure")) *>
          epicsSys.abortCmd.setTimeout[IO](DefaultTimeout) *>
          epicsSys.abortCmd.mark[IO] *>
          epicsSys.abortCmd.post[IO].void

      override def observeProgress(total: Time): Stream[IO, Progress] =
        ProgressUtil.countdown[IO](total, 0.seconds)

      override def calcTotalExposureTime(cfg: GnirsController.DCConfig): IO[Time] =
        GnirsController.calcTotalExposureTime[IO](cfg)
    }

  private val DefaultTimeout: Time = Seconds(60)
  private val ReadoutTimeout: Time = Seconds(300)
  private val ConfigTimeout: Time = Seconds(240)
}
