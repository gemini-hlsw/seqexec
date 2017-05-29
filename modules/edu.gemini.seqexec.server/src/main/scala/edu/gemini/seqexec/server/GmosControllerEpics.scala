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
import edu.gemini.spModel.gemini.gmos.GmosSouthType.{FPUnitSouth => FPU}
import edu.gemini.spModel.gemini.gmos.GmosSouthType.{StageModeSouth => StageMode}

import squants.Length

import scalaz._
import scalaz.Scalaz._
import scalaz.EitherT
import scalaz.concurrent.Task

object GmosControllerEpics extends GmosSouthController {
  private val Log = Logger.getLogger(getClass.getName)

  import EpicsCodex._

  val CC = GmosEpics.instance.configCmd
  val DC = GmosEpics.instance.configDCCmd

  override def getConfig: SeqAction[GmosSouthConfig] = ???

  implicit val ampReadModeEncoder: EncodeEpicsValue[AmpReadMode, String] = EncodeEpicsValue {
    case AmpReadMode.SLOW => "SLOW"
    case AmpReadMode.FAST => "FAST"
  }

  implicit val shutterStateEncoder: EncodeEpicsValue[ShutterState, String] = EncodeEpicsValue {
    case OpenShutter  => "OPEN"
    case CloseShutter => "CLOSED"
    case _            => ""
  }

  implicit val ampGainSettingEncoder: EncodeEpicsValue[AmpGainSetting, String] = EncodeEpicsValue(_.value.toString)

  implicit val ampCountEncoder: EncodeEpicsValue[AmpCount, String] = EncodeEpicsValue {
    // gmosAmpCount.lut
    case AmpCount.THREE  => ""
    case AmpCount.SIX    => "BEST"
    case AmpCount.TWELVE => "ALL"
  }

  implicit val binningEncoder: EncodeEpicsValue[Binning, Int] = EncodeEpicsValue { b => b.getValue }

  implicit val disperserEncoder: EncodeEpicsValue[Disperser, String] = EncodeEpicsValue(_.sequenceValue)

  implicit val disperserOrderEncoder: EncodeEpicsValue[DisperserOrder, String] = EncodeEpicsValue(_.sequenceValue)

  implicit val disperserLambdaEncoder: EncodeEpicsValue[Length, String] = EncodeEpicsValue((l: Length) => l.toNanometers.toString)

  implicit val beamEncoder: EncodeEpicsValue[Beam, String] = EncodeEpicsValue {
    case OutOfBeam => "OUT-OF-BEAM"
    case InBeam    => "IN-BEAM"
  }

  implicit val stageModeEncoder: EncodeEpicsValue[StageMode, String] = EncodeEpicsValue {
    case StageMode.NO_FOLLOW     => "MOVE"
    case StageMode.FOLLOW_XYZ    => "FOLLOW"
    case StageMode.FOLLOW_XY     => "FOLLOW-XY"
    case StageMode.FOLLOW_Z_ONLY => "FOLLOW-Z"
  }

  implicit val useElectronicOffsetEncoder: EncodeEpicsValue[UseElectronicOffset, Int] = EncodeEpicsValue(_.allow ? 1 | 0)

  private def gainSetting(ampMode: AmpReadMode, ampGain: AmpGain): AmpGainSetting = (ampMode, ampGain) match {
    // gmosAutoGain.lut
    case (AmpReadMode.SLOW, AmpGain.LOW)  => AmpGainSetting(0)
    case (AmpReadMode.SLOW, AmpGain.HIGH) => AmpGainSetting(0)
    case (AmpReadMode.FAST, AmpGain.LOW)  => AmpGainSetting(10)
    case (AmpReadMode.FAST, AmpGain.HIGH) => AmpGainSetting(0)
  }

  private def setShutterState(s: ShutterState): SeqAction[Unit] = s match {
    case UnsetShutter => SeqAction.void
    case s            => DC.setShutterState(encode(s))
  }

  private def roiNumUsed(s: RegionsOfInterest): Int = s match {
    case RegionsOfInterest(\/-(rois)) => rois.length
    case RegionsOfInterest(-\/(b))    => 1
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
    private def fromInt(xStart: Int, xSize: Int, yStart: Int, ySize: Int): Option[ROIValues] =
      (XStart.fromInt(xStart) |@| XSize.fromInt(xSize) |@| YStart.fromInt(yStart) |@| YSize.fromInt(ySize))(new ROIValues(_, _, _, _) {})

    // Built from OCS ROI values
    def fromOCS(roi: ROI): Option[ROIValues] =
      (XStart.fromInt(roi.getXStart) |@| XSize.fromInt(roi.getXSize) |@| YStart.fromInt(roi.getYStart) |@| YSize.fromInt(roi.getYSize))(new ROIValues(_, _, _, _) {})

    def builtInROI(b: BuiltinROI): Option[ROIValues] = b match {
      // gmosROI.lut
      case BuiltinROI.FULL_FRAME       => ROIValues.fromInt(xStart = 1, xSize = 6144, yStart = 1, ySize = 4224)
      case BuiltinROI.CCD2             => ROIValues.fromInt(xStart = 2049, xSize = 2048, yStart = 1, ySize = 4224)
      case BuiltinROI.CENTRAL_SPECTRUM => ROIValues.fromInt(xStart = 1, xSize = 6144, yStart = 1625, ySize = 1024)
      case BuiltinROI.CENTRAL_STAMP    => ROIValues.fromInt(xStart = 2923, xSize = 300, yStart = 1987, ySize = 300)
      case _                           => None
    }
  }

  private def setROI(binning: CCDBinning, s: RegionsOfInterest): SeqAction[Unit] = s match {
    case RegionsOfInterest(-\/(b))    => roiParameters(binning, 1, ROIValues.builtInROI(b))
    case RegionsOfInterest(\/-(rois)) => rois.zipWithIndex.map { case (roi, i) =>
      roiParameters(binning, i, ROIValues.fromOCS(roi))
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
    _ <- DC.setGainSetting(encode(gainSetting(dc.r.ampReadMode, dc.r.ampGain)))
    _ <- DC.setAmpCount(encode(dc.r.ampCount))
    _ <- DC.setRoiNumUsed(roiNumUsed(dc.roi))
    _ <- setROI(dc.bi, dc.roi)
    _ <- DC.setCcdXBinning(encode(dc.bi.x))
    _ <- DC.setCcdYBinning(encode(dc.bi.y))
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
      _ <- CC.setFilter1(filter1)
      _ <- CC.setFilter2(filter2)
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
      _ <- CC.setDisperser(disperser)
      _ <- CC.setDisperserMode(disperserMode)
      _ <- d.order.filter(_ => d.disperser != Disperser.MIRROR).fold(SeqAction.void)(o => CC.setDisperserOrder(encode(o)))
      _ <- d.lambda.filter(_ => d.disperser != Disperser.MIRROR && d.order.contains(Order.ZERO)).fold(SeqAction.void)(o => CC.setDisperserOrder(encode(o)))
    } yield ()
  }

  def setFPU(cc: GmosFPU): SeqAction[Unit] = {
    def builtInFPU(fpu: FPU): SeqAction[Unit] = {
      val (fpuName, beam: Option[Beam]) = fpu match {
        case FPU.FPU_NONE    => (none, OutOfBeam.some)
        case FPU.LONGSLIT_1  => ("0.25arcsec".some, InBeam.some)
        case FPU.LONGSLIT_2  => ("0.5arcsec".some, InBeam.some)
        case FPU.LONGSLIT_3  => ("0.75arcsec".some, InBeam.some)
        case FPU.LONGSLIT_4  => ("1.0arcsec".some, InBeam.some)
        case FPU.LONGSLIT_5  => ("1.5arcsec".some, InBeam.some)
        case FPU.LONGSLIT_6  => ("2.0arcsec".some, InBeam.some)
        case FPU.LONGSLIT_7  => ("5.0arcsec".some, InBeam.some)
        case FPU.IFU_1       => ("IFU-2".some, InBeam.some)
        case FPU.IFU_2       => ("IFU-B".some, InBeam.some)
        case FPU.IFU_3       => ("IFU-R".some, InBeam.some)
        case FPU.BHROS       => (none, none)
        case FPU.IFU_N       => ("IFU-NS-2".some, InBeam.some)
        case FPU.IFU_N_B     => ("IFU-NS-B".some, InBeam.some)
        case FPU.IFU_N_R     => ("IFU-NS-R".some, InBeam.some)
        case FPU.NS_1        => ("NS0.5arcsec".some, InBeam.some)
        case FPU.NS_2        => ("NS0.75arcsec".some, InBeam.some)
        case FPU.NS_3        => ("NS1.0arcsec".some, InBeam.some)
        case FPU.NS_4        => ("NS1.5arcsec".some, InBeam.some)
        case FPU.NS_5        => ("NS2.0arcsec".some, InBeam.some)
        case FPU.CUSTOM_MASK => (none, none)
      }
      for {
        _ <- fpuName.fold(SeqAction.void)(CC.setFpu)
        _ <- beam.fold(SeqAction.void)(b => CC.setInBeam(encode(b)))
      } yield ()
    }

    def customFPU(name: String): SeqAction[Unit] = {
      val (fpuName, beam: Option[Beam]) = name match {
        case "None" => (none, none)
        case _      => (name.some, InBeam.some)
      }
      for {
        _ <- fpuName.fold(SeqAction.void)(CC.setFpu)
        _ <- beam.fold(SeqAction.void)(b => CC.setInBeam(encode(b)))
      } yield ()
    }

    cc match {
      case UnknownFPU          => SeqAction.void
      case BuiltInFPU(fpu)     => builtInFPU(fpu)
      case CustomMaskFPU(name) => customFPU(name)
    }
  }

  def setCCConfig(cc: CCConfig): SeqAction[Unit] = for {
    _ <- setFilters(cc.filter)
    _ <- setDisperser(cc.disperser)
    _ <- setFPU(cc.fpu)
    _ <- CC.setStageMode(encode(cc.stage))
    // TODO Is DTaX channel a double?
    _ <- CC.setDtaXOffset(cc.dtaX.toString)
    _ <- cc.useElectronicOffset.fold(CC.setElectronicOffsetting(0))(e => CC.setElectronicOffsetting(encode(e)))
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
    _ <- GmosEpics.instance.observeCmd.setLabel(obsid)
    _ <- GmosEpics.instance.observeCmd.post
    _ <- EitherT(Task(Log.info("Completed Gmos observation").right))
  } yield obsid
}
