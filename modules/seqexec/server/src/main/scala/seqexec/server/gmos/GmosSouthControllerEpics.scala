// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.effect._
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import seqexec.server.EpicsCodex.EncodeEpicsValue
import seqexec.server.gmos.GmosController.Config.Beam
import seqexec.server.gmos.GmosController.{SouthTypes, southConfigTypes}
import seqexec.server.gmos.GmosControllerEpics.ROIValues
import edu.gemini.spModel.gemini.gmos.GmosCommonType
import edu.gemini.spModel.gemini.gmos.GmosCommonType.{AmpGain, AmpReadMode, BuiltinROI}
import edu.gemini.spModel.gemini.gmos.GmosSouthType.{DisperserSouth => Disperser, FPUnitSouth => FPU, FilterSouth => Filter, StageModeSouth => StageMode}

object GmosSouthEncoders extends GmosControllerEpics.Encoders[SouthTypes] {
  override val disperser: EncodeEpicsValue[SouthTypes#Disperser, String] = EncodeEpicsValue{
    case Disperser.MIRROR      => "mirror"
    case Disperser.B1200_G5321 => "B1200+_G5321"
    case Disperser.R831_G5322  => "R831+_G5322"
    case Disperser.B600_G5323  => "B600+_G5323"
    case Disperser.R600_G5324  => "R600+_G5324"
    case Disperser.R400_G5325  => "R400+_G5325"
    case Disperser.R150_G5326  => "R150+_G5326"
  }

  override val fpu: EncodeEpicsValue[SouthTypes#FPU, (Option[String], Option[String])] = EncodeEpicsValue{ a => {
    val r = a match {
      case FPU.FPU_NONE    => (none, Beam.OutOfBeam.some)
      case FPU.LONGSLIT_1  => ("0.25arcsec".some, Beam.InBeam.some)
      case FPU.LONGSLIT_2  => ("0.5arcsec".some, Beam.InBeam.some)
      case FPU.LONGSLIT_3  => ("0.75arcsec".some, Beam.InBeam.some)
      case FPU.LONGSLIT_4  => ("1.0arcsec".some, Beam.InBeam.some)
      case FPU.LONGSLIT_5  => ("1.5arcsec".some, Beam.InBeam.some)
      case FPU.LONGSLIT_6  => ("2.0arcsec".some, Beam.InBeam.some)
      case FPU.LONGSLIT_7  => ("5.0arcsec".some, Beam.InBeam.some)
      case FPU.IFU_1       => ("IFU-2".some, Beam.InBeam.some)
      case FPU.IFU_2       => ("IFU-B".some, Beam.InBeam.some)
      case FPU.IFU_3       => ("IFU-R".some, Beam.InBeam.some)
      case FPU.BHROS       => (none, none)
      case FPU.IFU_N       => ("IFU-NS-2".some, Beam.InBeam.some)
      case FPU.IFU_N_B     => ("IFU-NS-B".some, Beam.InBeam.some)
      case FPU.IFU_N_R     => ("IFU-NS-R".some, Beam.InBeam.some)
      case FPU.NS_1        => ("NS0.5arcsec".some, Beam.InBeam.some)
      case FPU.NS_2        => ("NS0.75arcsec".some, Beam.InBeam.some)
      case FPU.NS_3        => ("NS1.0arcsec".some, Beam.InBeam.some)
      case FPU.NS_4        => ("NS1.5arcsec".some, Beam.InBeam.some)
      case FPU.NS_5        => ("NS2.0arcsec".some, Beam.InBeam.some)
      case FPU.CUSTOM_MASK => (none, none)
    }
    (r._1, r._2.map(GmosControllerEpics.beamEncoder.encode))
  } }

  override val filter: EncodeEpicsValue[SouthTypes#Filter, (String, String)] = EncodeEpicsValue{
    case Filter.Z_G0343                 => ("Z_G0343", "open2-8")
    case Filter.Y_G0344                 => ("Y_G0344", "open2-8")
    case Filter.HeII_G0340              => ("HeII_G0340", "open2-8")
    case Filter.HeIIC_G0341             => ("open1-6", "HeIIC_G0341")
    case Filter.SII_G0335               => ("open1-6", "SII_G0335")
    case Filter.Ha_G0336                => ("open1-6", "Ha_G0336")
    case Filter.HaC_G0337               => ("open1-6", "HaC_G0337")
    case Filter.OIII_G0338              => ("open1-6", "OIII_G0338")
    case Filter.OIIIC_G0339             => ("open1-6", "OIIIC_G0339")
    case Filter.OVI_G0347               => ("OVI_G0347", "open2-8")
    case Filter.OVIC_G0348              => ("OVIC_G0348", "open2-8")
    case Filter.u_G0332                 => ("open1-6", "u_G0332")
    case Filter.g_G0325                 => ("open1-6", "g_G0325")
    case Filter.r_G0326                 => ("open1-6", "r_G0326")
    case Filter.i_G0327                 => ("open1-6", "i_G0327")
    case Filter.z_G0328                 => ("open1-6", "z_G0328")
    case Filter.GG455_G0329             => ("GG455_G0329", "open2-8")
    case Filter.OG515_G0330             => ("OG515_G0330", "open2-8")
    case Filter.RG610_G0331             => ("RG610_G0331", "open2-8")
    case Filter.CaT_G0333               => ("CaT_G0333", "open2-8")
    case Filter.HartmannA_G0337_r_G0326 => ("HartmannA_G0337", "r_G0326")
    case Filter.HartmannB_G0338_r_G0326 => ("HartmannB_G0338", "r_G0326")
    case Filter.g_G0325_GG455_G0329     => ("GG455_G0329", "g_G0325")
    case Filter.g_G0325_OG515_G0330     => ("OG515_G0330", "g_G0325")
    case Filter.r_G0326_RG610_G0331     => ("RG610_G0331", "r_G0326")
    case Filter.i_G0327_CaT_G0333       => ("CaT_G0333", "i_G0327")
    case Filter.i_G0327_RG780_G0334     => ("RG780_G0334", "i_G0327")
    case Filter.z_G0328_CaT_G0333       => ("CaT_G0333", "z_G0328")
    case Filter.RG780_G0334             => ("open1-6", "RG780_G0334")
    case Filter.Lya395_G0342            => ("open1-6", "Lya395_G0342")
    case Filter.NONE                    => ("open1-6", "open2-8")
  }

  override val stageMode: EncodeEpicsValue[SouthTypes#GmosStageMode, String] = EncodeEpicsValue {
    case StageMode.NO_FOLLOW     => "MOVE"
    case StageMode.FOLLOW_XYZ    => "FOLLOW"
    case StageMode.FOLLOW_XY     => "FOLLOW-XY"
    case StageMode.FOLLOW_Z_ONLY => "FOLLOW-Z"
  }

  override val builtInROI: EncodeEpicsValue[BuiltinROI, Option[ROIValues]] = EncodeEpicsValue {
    case BuiltinROI.FULL_FRAME       => ROIValues.fromInt(xStart = 1, xSize = 6144, yStart = 1, ySize = 4224)
    case BuiltinROI.CCD2             => ROIValues.fromInt(xStart = 2049, xSize = 2048, yStart = 1, ySize = 4224)
    case BuiltinROI.CENTRAL_SPECTRUM => ROIValues.fromInt(xStart = 1, xSize = 6144, yStart = 1625, ySize = 1024)
    case BuiltinROI.CENTRAL_STAMP    => ROIValues.fromInt(xStart = 2923, xSize = 300, yStart = 1987, ySize = 300)
    case _                           => None
  }

  override val autoGain: EncodeEpicsValue[(GmosCommonType.AmpReadMode, GmosCommonType.AmpGain), Int] = {
    // gmosAutoGain.lut
    case (AmpReadMode.SLOW, AmpGain.LOW)  => 0
    case (AmpReadMode.SLOW, AmpGain.HIGH) => 0
    case (AmpReadMode.FAST, AmpGain.LOW)  => 10
    case (AmpReadMode.FAST, AmpGain.HIGH) => 0
  }
}

object GmosSouthControllerEpics {
  def apply[F[_]: Async: Timer: Logger](sys: => GmosEpics[F]): GmosController[F, SouthTypes] = {
    implicit val encoders = GmosSouthEncoders
    GmosControllerEpics[F, SouthTypes](sys, southConfigTypes)
  }
}
