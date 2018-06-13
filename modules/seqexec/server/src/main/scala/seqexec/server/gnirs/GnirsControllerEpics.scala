// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import cats.Eq
import cats.data.EitherT
import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server._
import edu.gemini.spModel.gemini.gnirs.GNIRSParams
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.{Camera, Decker, Disperser, ReadMode}
import org.log4s.getLogger
import squants.{Length, Seconds, Time}
import squants.space.LengthConversions._
import squants.electro.Millivolts

import scala.math.abs
import cats.implicits._

object GnirsControllerEpics extends GnirsController {
  private val Log = getLogger

  import GnirsController._
  import EpicsCodex._

  private def epicsSys = GnirsEpics.instance
  private def ccCmd = epicsSys.configCCCmd
  private def dcCmd = epicsSys.configDCCmd

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

  private def autoFilter(w: Length): GnirsController.Filter2Pos = {
    val table = List(
      GnirsController.Filter2Pos.X -> 1.17,
      GnirsController.Filter2Pos.J -> 1.42,
      GnirsController.Filter2Pos.H -> 1.86,
      GnirsController.Filter2Pos.K -> 2.70,
      GnirsController.Filter2Pos.L -> 4.30,
      GnirsController.Filter2Pos.M -> 6.0
    ).map{ case (f, w) => (f, w.nanometers) }

    table.foldRight[GnirsController.Filter2Pos](GnirsController.Filter2Pos.XD){ case (t, v) => if(w < t._2) t._1 else v}
  }

  private def setFilter2(f: Filter2, w: Wavelength): List[SeqAction[Unit]] = {
    val pos = f match {
      case Manual(p) => p
      case Auto      => autoFilter(w)
    }
    smartSetParam(encode(pos), epicsSys.filter2.map(removePartName), ccCmd.setFilter2(encode(pos)))

  }

  private def setOtherCCParams(config: Other): List[SeqAction[Unit]] = {
    val open = "Open"
    val focus = "best focus"
    val wavelengthTolerance = 0.0005
    val refocusParams = setAcquisitionMirror(config.mode) ++
      smartSetParam(encode(config.filter1), epicsSys.filter1.map(removePartName), ccCmd.setFilter1(encode(config.filter1))) ++
      setFilter2(config.filter2, config.wavel) ++
      setSpectrographyComponents(config.mode, config.camera) ++
      smartSetParam(encode(config.camera), epicsSys.camera.map(removePartName), ccCmd.setCamera(encode(config.camera)))
    // Force focus configuration if any of the above is set
    val focusSet = if (refocusParams.nonEmpty) List(ccCmd.setFocusBest(focus)) else Nil

    smartSetParam(open, epicsSys.cover.map(removePartName), ccCmd.setCover(open)) ++
      refocusParams ++
      focusSet ++
      config.slitWidth.map(sl => smartSetParam(encode(sl), epicsSys.slitWidth.map(removePartName), ccCmd.setSlitWidth(encode(sl)))).getOrElse(Nil) ++
      smartSetParam(encode(config.decker), epicsSys.decker.map(removePartName), ccCmd.setDecker(encode(config.decker))) ++
      smartSetDoubleParam(wavelengthTolerance)(encode(config.wavel), epicsSys.centralWavelength, ccCmd.setCentralWavelength(encode(config.wavel)))

  }

  private def setCCParams(config: CCConfig): SeqAction[EpicsCommand.Result] = {
    val params = config match {
      case Dark    => setDarkCCParams
      case c:Other => setOtherCCParams(c)
    }
    if (params.isEmpty) SeqAction(EpicsCommand.Completed)
    else params.sequence.map(_ => ()) *>
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
    SeqAction(Log.debug("Starting GNIRS configuration")) *>
      setDCParams(config.dc) *>
      setCCParams(config.cc) *>
      SeqAction(Log.debug("Completed GNIRS configuration"))


  override def observe(fileId: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result] = for {
    _   <- GnirsEpics.instance.observeCmd.setLabel(fileId)
    _   <- GnirsEpics.instance.observeCmd.setTimeout(expTime + ReadoutTimeout)
    ret <- GnirsEpics.instance.observeCmd.post
  } yield ret

  override def endObserve: SeqAction[Unit] = for {
    _ <- EitherT.right(IO(Log.debug("Send endObserve to GNIRS")))
    _ <- GnirsEpics.instance.endObserveCmd.setTimeout(DefaultTimeout)
    _ <- GnirsEpics.instance.endObserveCmd.mark
    _ <- GnirsEpics.instance.endObserveCmd.post
  } yield ()

  override def stopObserve: SeqAction[Unit] = for {
    _ <- EitherT.right(IO(Log.info("Stop GNIRS exposure")))
    _ <- GnirsEpics.instance.stopCmd.setTimeout(DefaultTimeout)
    _ <- GnirsEpics.instance.stopCmd.mark
    _ <- GnirsEpics.instance.stopCmd.post
  } yield ()

  override def abortObserve: SeqAction[Unit] = for {
    _ <- EitherT.right(IO(Log.info("Abort GNIRS exposure")))
    _ <- GnirsEpics.instance.abortCmd.setTimeout(DefaultTimeout)
    _ <- GnirsEpics.instance.abortCmd.mark
    _ <- GnirsEpics.instance.abortCmd.post
  } yield ()

  private def removePartName(s: String) = {
    val pattern = "_G[0-9]{4}$"

    s.replaceAll(pattern, "")
  }

  private def smartSetParam[A: Eq](v: A, get: => Option[A], set: SeqAction[Unit]): List[SeqAction[Unit]] =
    if(get =!= v.some) List(set) else Nil
  private def smartSetDoubleParam(tolerance: Double)(v: Double, get: => Option[Double], set: SeqAction[Unit]): List[SeqAction[Unit]] =
    if(get.forall(x => abs(x - v) < tolerance)) List(set) else Nil

  private val DefaultTimeout: Time = Seconds(60)
  private val ReadoutTimeout: Time = Seconds(300)
  private val ConfigTimeout: Time = Seconds(240)
}
