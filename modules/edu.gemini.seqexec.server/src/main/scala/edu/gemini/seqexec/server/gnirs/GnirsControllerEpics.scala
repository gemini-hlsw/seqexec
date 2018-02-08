// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gnirs

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.EpicsCodex.EncodeEpicsValue
import edu.gemini.seqexec.server.{EpicsCodex, EpicsCommand, ObserveCommand, SeqAction}
import edu.gemini.spModel.gemini.gnirs.GNIRSParams
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.{Camera, Decker, Disperser, ReadMode}
import org.log4s.getLogger
import squants.{Seconds, Time}
import squants.electro.Millivolts

import scalaz.concurrent.Task
import scalaz._
import Scalaz._

object GnirsControllerEpics extends GnirsController {
  private val Log = getLogger

  import GnirsController._
  import EpicsCodex._

  private val epicsSys = GnirsEpics.instance
  private val ccCmd = epicsSys.configCCCmd
  private val dcCmd = epicsSys.configDCCmd

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
  }

  implicit val filter2Encoder: EncodeEpicsValue[Filter2, String] = EncodeEpicsValue {
    case Filter2.H    => "H"
    case Filter2.H2   => "H2"
    case Filter2.J    => "J"
    case Filter2.K    => "K"
    case Filter2.L    => "L"
    case Filter2.M    => "M"
    case Filter2.Open => "Open"
    case Filter2.PAH  => "PAH"
    case Filter2.X    => "X"
    case Filter2.XD   => "XD"
  }

  private def setAcquisitionMirror(mode: Mode): List[SeqAction[Unit]] = {
    val v = mode match {
      case Acquisition => "In"
      case _           => "Out"
    }

    smartSetParam(v, epicsSys.acqMirror.map(removePartName), ccCmd.setAcqMirror(v))
  }

  private def setGrating(s: Spectrography, c: Camera): List[SeqAction[Unit]] = {
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

    smartSetParam(v, epicsSys.grating.map(removePartName), ccCmd.setGrating(v)) ++
      smartSetParam(defaultMode, epicsSys.gratingMode, ccCmd.setGratingMode(defaultMode))
  }

  private def setPrism(s: Spectrography, c: Camera): List[SeqAction[Unit]] = {
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

    smartSetParam(v, epicsSys.prism.map(removePartName), ccCmd.setPrism(v))
  }

  private def setDarkCCParams: List[SeqAction[Unit]] = {
    val closed = "Closed"
    val darkFilter = "Dark"
    smartSetParam(closed, epicsSys.cover.map(removePartName), ccCmd.setCover(closed)) ++
      smartSetParam(darkFilter, epicsSys.filter1.map(removePartName), ccCmd.setFilter1(darkFilter))
  }

  private def setSpectrographyComponents(mode: Mode, c: Camera): List[SeqAction[Unit]] = mode match {
    case Acquisition => Nil
    case s:Spectrography => setGrating(s, c) ++ setPrism(s, c)
  }

  private def setOtherCCParams(config: Other): List[SeqAction[Unit]] = {
    val open = "Open"
    val focus = "best focus"
    val refocusParams = setAcquisitionMirror(config.mode) ++
      smartSetParam(encode(config.filter1), epicsSys.filter1.map(removePartName), ccCmd.setFilter1(encode(config.filter1))) ++
      smartSetParam(encode(config.filter2), epicsSys.filter2.map(removePartName), ccCmd.setFilter2(encode(config.filter2))) ++
      setSpectrographyComponents(config.mode, config.camera) ++
      smartSetParam(encode(config.camera), epicsSys.camera.map(removePartName), ccCmd.setCamera(encode(config.camera)))
    // Force focus configuration if any of the above is set
    val focusSet = if (epicsSys.focus =/= focus.some || !refocusParams.isEmpty) List(ccCmd.setFocus(focus)) else Nil

    smartSetParam(open, epicsSys.cover.map(removePartName), ccCmd.setCover(open)) ++
      refocusParams ++
      focusSet ++
      config.slitWidth.map(sl => smartSetParam(encode(sl), epicsSys.slitWidth.map(removePartName), ccCmd.setSlitWidth(encode(sl)))).getOrElse(Nil) ++
      smartSetParam(encode(config.decker), epicsSys.decker.map(removePartName), ccCmd.setDecker(encode(config.decker)))

  }

  private def setCCParams(config: CCConfig): SeqAction[EpicsCommand.Result] = {
    val params = config match {
      case Dark    => setDarkCCParams
      case c:Other => setOtherCCParams(c)
    }
    if (params.isEmpty) SeqAction(EpicsCommand.Completed)
    else params.sequenceU.map(_ => ()) *>
      ccCmd.setTimeout(ConfigTimeout) *>
      ccCmd.post
  }

  private def setDCParams(config: DCConfig): SeqAction[EpicsCommand.Result] = {
    val (lowNoise, digitalAvgs) = readModeEncoder.encode(config.readMode)

    dcCmd.setExposureTime(config.exposureTime.toSeconds) *>
      dcCmd.setCoadds(config.coadds) *>
      dcCmd.setDetBias(encode(config.wellDepth)) *>
      dcCmd.setLowNoise(lowNoise) *>
      dcCmd.setDigitalAvgs(digitalAvgs)*>
      dcCmd.setTimeout(DefaultTimeout) *>
      dcCmd.post
  }

  override def applyConfig(config: GnirsConfig): SeqAction[Unit] =
    SeqAction(Log.info("Starting GNIRS configuration")) *>
      setDCParams(config.dc) *>
      setCCParams(config.cc) *>
      SeqAction(Log.info("Completed GNIRS configuration"))


  override def observe(fileId: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] = for {
    _   <- GnirsEpics.instance.observeCmd.setLabel(fileId)
    _   <- GnirsEpics.instance.observeCmd.setTimeout(expTime + ReadoutTimeout)
    ret <- GnirsEpics.instance.observeCmd.post
  } yield ret

  override def endObserve: SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Send endObserve to GNIRS").right))
    _ <- GnirsEpics.instance.endObserveCmd.setTimeout(DefaultTimeout)
    _ <- GnirsEpics.instance.endObserveCmd.mark
    _ <- GnirsEpics.instance.endObserveCmd.post
  } yield ()

  override def stopObserve: SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Stop GNIRS exposure").right))
    _ <- GnirsEpics.instance.stopCmd.setTimeout(DefaultTimeout)
    _ <- GnirsEpics.instance.stopCmd.mark
    _ <- GnirsEpics.instance.stopCmd.post
  } yield ()

  override def abortObserve: SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Abort GNIRS exposure").right))
    _ <- GnirsEpics.instance.abortCmd.setTimeout(DefaultTimeout)
    _ <- GnirsEpics.instance.abortCmd.mark
    _ <- GnirsEpics.instance.abortCmd.post
  } yield ()

  private def removePartName(s: String) = {
    val pattern = "_G[0-9]{4}$"

    s.replaceAll(pattern, "")
  }

  private def smartSetParam[A: Equal](v: A, get: => Option[A], set: SeqAction[Unit]): List[SeqAction[Unit]] =
    if(get =/= v.some) List(set) else Nil

  private val DefaultTimeout: Time = Seconds(60)
  private val ReadoutTimeout: Time = Seconds(300)
  private val ConfigTimeout: Time = Seconds(240)
}
