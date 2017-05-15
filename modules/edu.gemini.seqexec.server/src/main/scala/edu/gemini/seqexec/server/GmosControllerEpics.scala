package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.GmosSouthController._
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpReadMode
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpGain
import edu.gemini.spModel.gemini.gmos.GmosCommonType.AmpCount
import edu.gemini.spModel.gemini.gmos.GmosCommonType.BuiltinROI
import edu.gemini.spModel.gemini.gmos.GmosCommonType.ROIDescription

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

  private def builtInROI(b: BuiltinROI): ROI = b match {
    case BuiltinROI.FULL_FRAME       => ROI(xStart = 1, xSize = 6144, yStart = 1, ySize = 4608)
    case BuiltinROI.CCD2             => ROI(xStart = 2049, xSize = 2048, yStart = 1, ySize = 4608)
    case BuiltinROI.CENTRAL_SPECTRUM => ROI(xStart = 1, xSize = 6144, yStart = 1793, ySize = 1024)
    case BuiltinROI.CENTRAL_STAMP    => ROI(xStart = 2923, xSize = 300, yStart = 2155, ySize = 300)
    case _                           => ROI(xStart = 0, xSize = 0, yStart = 0, ySize = 0)
  }

  private def setROI(s: RegionsOfInterest): SeqAction[Unit] = s match {
    case RegionsOfInterest(b, _) if b != BuiltinROI.CUSTOM => roiParameters(1, builtInROI(b))
    // TODO Support custom ROIs
    case RegionsOfInterest(b, rois)                        => SeqAction.void
  }

  private def roiParameters(index: Int, roi: ROI): SeqAction[Unit] = {
    GmosEpics.instance.configDCCmd.rois.get(index).map { r =>
      for {
        _ <- r.setCcdXstart1(roi.xStart)
        _ <- r.setCcdXsize1(roi.xSize)
        _ <- r.setCcdYstart1(roi.yStart)
        _ <- r.setCcdYsize1(roi.ySize)
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
    _ <- setROI(dc.roi)
    _ <- GmosEpics.instance.configDCCmd.setCcdXBinning(encode(dc.bi.x))
    _ <- GmosEpics.instance.configDCCmd.setCcdYBinning(encode(dc.bi.y))
  } yield ()

  override def applyConfig(config: GmosSouthConfig): SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Start Gmos configuration").right))
    _ <- setDCConfig(config.dc)
    // _ <- setCCConfig(config.cc//)
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
