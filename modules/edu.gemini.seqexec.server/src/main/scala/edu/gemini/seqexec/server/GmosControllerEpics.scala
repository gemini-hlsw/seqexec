package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.GmosSouthController._
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpReadMode
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpGain
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpCount
import edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
import edu.gemini.spModel.gemini.gmos.GmosCommonType.ROIDescription
import edu.gemini.spModel.gemini.gmos.GmosCommonType.Order
import edu.gemini.spModel.gemini.gmos.GmosSouthType.{FilterSouth => Filter}
import edu.gemini.spModel.gemini.gmos.GmosSouthType.{DisperserSouth => Disperser}

import squants.Length

import scalaz.Scalaz._
import scalaz.EitherT
import scalaz.concurrent.Task

object GmosControllerEpics extends GmosSouthController {
  private val Log = Logger.getLogger(getClass.getName)

  import EpicsCodex._

  override def getConfig: SeqAction[GmosSouthConfig] = ???

  implicit val ampReadModeEncoder: EncodeEpicsValue[AmpReadMode, String] = EncodeEpicsValue {
    // Taken from gmosAmpReadMode.lut
    case AmpReadMode.SLOW => "SLOW"
    case AmpReadMode.FAST => "FAST"
  }

  implicit val shutterStateEncoder: EncodeEpicsValue[ShutterState, String] = EncodeEpicsValue {
    case OpenShutter  => "OPEN"
    case CloseShutter => "CLOSED"
    case _            => ""
  }

  implicit val ampGainSettingEncoder: EncodeEpicsValue[AmpGainSetting, String] = EncodeEpicsValue(_.value.toString)

  implicit val ampCountEncoder: EncodeEpicsValue[AmpCount, Int] = EncodeEpicsValue {
    case AmpCount.THREE  => 3
    case AmpCount.SIX    => 6
    case AmpCount.TWELVE => 12
  }

  implicit val binningEncoder: EncodeEpicsValue[Binning, Int] = EncodeEpicsValue { b => b.getValue() }

  implicit val disperserEncoder: EncodeEpicsValue[Disperser, String] = EncodeEpicsValue(_.sequenceValue)

  implicit val disperserOrderEncoder: EncodeEpicsValue[DisperserOrder, String] = EncodeEpicsValue(_.sequenceValue)

  implicit val disperserLambdaEncoder: EncodeEpicsValue[Length, String] = EncodeEpicsValue((l: Length) => l.toNanometers.toString)

  private def gainSetting(ampMode: AmpReadMode, ampGain: AmpGain): AmpGainSetting = (ampMode, ampGain) match {
    case (AmpReadMode.SLOW, AmpGain.LOW)  => AmpGainSetting(2)
    case (AmpReadMode.SLOW, AmpGain.HIGH) => AmpGainSetting(1)
    case (AmpReadMode.FAST, AmpGain.LOW)  => AmpGainSetting(6)
    case (AmpReadMode.FAST, AmpGain.HIGH) => AmpGainSetting(5)
  }

  private def setShutterState(s: ShutterState): SeqAction[Unit] = s match {
    case UnsetShutter => SeqAction.void
    case s            => GmosEpics.instance.configDCCmd.setShutterState(encode(s))
  }

  private def roiNumUsed(s: RegionsOfInterest): Int = s match {
    case RegionsOfInterest(b, _) if b != BuiltinROI.CUSTOM => 1
    case RegionsOfInterest(b, rois)                        => rois.length
  }

  case class ROI(xStart: Int, xSize: Int, yStart: Int, ySize: Int)

  private def builtInROI(binning: CCDBinning, b: BuiltinROI): ROI = b match {
    case BuiltinROI.FULL_FRAME       => ROI(xStart = 1, xSize = 6144, yStart = 1, ySize = 4608)
    case BuiltinROI.CCD2             => ROI(xStart = 2049, xSize = 2048, yStart = 1, ySize = 4608)
    case BuiltinROI.CENTRAL_SPECTRUM => ROI(xStart = 1, xSize = 6144, yStart = 1793, ySize = 1024)
    case BuiltinROI.CENTRAL_STAMP    => ROI(xStart = 2923, xSize = 300, yStart = 2155, ySize = 300)
    case _                           => ROI(xStart = 0, xSize = 0, yStart = 0, ySize = 0)
  }

  private def setROI(binning: CCDBinning, s: RegionsOfInterest): SeqAction[Unit] = s match {
    case RegionsOfInterest(b, _) if b != BuiltinROI.CUSTOM => roiParameters(binning, 1, builtInROI(binning, b))
    // TODO Support custom ROIs
    case RegionsOfInterest(b, rois)                        => SeqAction.void
  }

  private def roiParameters(binning: CCDBinning, index: Int, roi: ROI): SeqAction[Unit] = {
    GmosEpics.instance.configDCCmd.rois.get(index).map { r =>
      for {
        _ <- r.setCcdXstart1(roi.xStart)
        _ <- r.setCcdXsize1(roi.xSize / binning.x.getValue)
        _ <- r.setCcdYstart1(roi.yStart)
        _ <- r.setCcdYsize1(roi.ySize / binning.y.getValue)
      } yield ()
    }.fold(SeqAction.void)(identity)
  }

  def setDCConfig(dc: DCConfig): SeqAction[Unit] = for {
    // TODO nsRow, nsPairs
    _ <- GmosEpics.instance.configDCCmd.setExposureTime(dc.t)
    // TODO Bias time?
    _ <- setShutterState(dc.s)
    _ <- GmosEpics.instance.configDCCmd.setAmpReadMode(encode(dc.r.ampReadMode))
    _ <- GmosEpics.instance.configDCCmd.setGainSetting(encode(gainSetting(dc.r.ampReadMode, dc.r.ampGain)))
    _ <- GmosEpics.instance.configDCCmd.setAmpCount(encode(dc.r.ampCount))
    _ <- GmosEpics.instance.configDCCmd.setRoiNumUsed(roiNumUsed(dc.roi))
    _ <- setROI(dc.bi, dc.roi)
    _ <- GmosEpics.instance.configDCCmd.setCcdXBinning(encode(dc.bi.x))
    _ <- GmosEpics.instance.configDCCmd.setCcdYBinning(encode(dc.bi.y))
  } yield ()

  def setFilters(f: Filter): SeqAction[Unit] = {
    val (filter1, filter2) = f match {
      case Filter.Z_G0343       => ("Z_G0343", "open2-8")
      case Filter.Y_G0344       => ("Y_G0344", "open2-8")
      case Filter.HeII_G0340    => ("HeII_G0340", "open2-8")
      case Filter.HeIIC_G0341   => ("HeIIC_G0341", "open2-8")
      case Filter.SII_G0335     => ("open1-6", "SII_G0335")
      case Filter.Ha_G0336      => ("open1-6", "Ha_G0336")
      case Filter.HaC_G0337     => ("open1-6", "HaC_G0337")
      case Filter.OIII_G0338    => ("open1-6", "OIII_G0338")
      case Filter.OIIIC_G0339   => ("open1-6", "OIIIC_G0339")
      case Filter.u_G0332       => ("open1-6", "u_G0332")
      case Filter.g_G0325       => ("open1-6", "g_G0325")
      case Filter.r_G0326       => ("open1-6", "r_G0326")
      case Filter.i_G0327       => ("open1-6", "i_G0327")
      case Filter.z_G0328       => ("open1-6", "z_G0328")
      case Filter.GG455_G0329   => ("GG455_G0329", "open2-8")
      case Filter.OG515_G0330   => ("OG515_G0330", "open2-8")
      case Filter.RG610_G0331   => ("RG610_G0331", "open2-8")
      case Filter.CaT_G0333     => ("CaT_G0333", "open2-8")
      case Filter.HartmannA_G0337_r_G0326 => ("HartmannA_G0337", "r_G0326")
      case Filter.HartmannB_G0338_r_G0326 => ("HartmannB_G0338", "r_G0326")
      case Filter.g_G0325_GG455_G0329     => ("GG455_G0329", "g_G0325")
      case Filter.g_G0325_OG515_G0330     => ("OG515_G0330", "g_G0325")
      case Filter.r_G0326_RG610_G0331     => ("RG610_G0331", "r_G0326")
      case Filter.i_G0327_CaT_G0333       => ("CaT_G0333", "i_G0327")
      case Filter.i_G0327_RG780_G0334     => ("CaT_G0333", "i_G0327")
      case Filter.z_G0328_CaT_G0333       => ("RG780_G0334", "i_G0327")
      case Filter.RG780_G0334    => ("RG780_G0334", "open2-8")
      case Filter.Lya395_G0342   => ("open1-6", "Lya395_G0342")
      case Filter.NONE           => ("open1-6", "open2-8")
    }
    for {
      _ <- GmosEpics.instance.configCmd.setFilter1(filter1)
      _ <- GmosEpics.instance.configCmd.setFilter2(filter2)
    } yield ()
  }

  def setDisperser(d: GmosDisperser): SeqAction[Unit] = {
    val disperser = d.disperser match {
      case Disperser.MIRROR      => "mirror"
      case Disperser.B1200_G5321 => "B1200+_G5321"
      case Disperser.R831_G5322  => "R831+_G5322"
      case Disperser.B600_G5323  => "B600+_G5323"
      case Disperser.R600_G5324  => "R600+_G5324"
      case Disperser.R400_G5325  => "R400+_G5325"
      case Disperser.R150_G5326  => "R150+_G5326"
    }
    val disperserMode = "Select Grating and Tilt"
    for {
      _ <- GmosEpics.instance.configCmd.setDisperser(disperser)
      _ <- GmosEpics.instance.configCmd.setDisperserMode(disperserMode)
      _ <- d.order.filter(_ => d.disperser != Disperser.MIRROR).fold(SeqAction.void)(o => GmosEpics.instance.configCmd.setDisperserOrder(encode(o)))
      _ <- d.lambda.filter(_ => d.disperser != Disperser.MIRROR && d.order.exists(_ == Order.ZERO)).fold(SeqAction.void)(o => GmosEpics.instance.configCmd.setDisperserOrder(encode(o)))
    } yield ()
  }

  def setCCConfig(cc: CCConfig): SeqAction[Unit] = for {
    _ <- setFilters(cc.filter)
    _ <- setDisperser(cc.disperser)
  } yield ()

  override def applyConfig(config: GmosSouthConfig): SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Start Gmos configuration").right))
    _ <- setDCConfig(config.dc)
    _ <- setCCConfig(config.cc)
    _ <- GmosEpics.instance.post
    _ <- EitherT(Task(Log.info("Completed Gmos configuration").right))
  } yield ()

  override def observe(obsid: ImageFileId): SeqAction[ImageFileId] = for {
    _ <- EitherT(Task(Log.info("Start Gmos observation").right))
    // _ <- Gmos.instance.observeCmd.setLabel(obsid)
    // _ <- Gmos.instance.observeCmd.post
    _ <- EitherT(Task(Log.info("Completed Gmos observation").right))
  } yield obsid
}
