// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.effect._
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import seqexec.server.EpicsCodex
import seqexec.server.EpicsCodex.EncodeEpicsValue
import seqexec.server.gmos.GmosController.Config.Beam
import seqexec.server.gmos.GmosController.{NorthTypes, northConfigTypes}
import seqexec.server.gmos.GmosControllerEpics.ROIValues
import edu.gemini.spModel.gemini.gmos.GmosCommonType
import edu.gemini.spModel.gemini.gmos.GmosCommonType.{AmpGain, AmpReadMode, BuiltinROI}
import edu.gemini.spModel.gemini.gmos.GmosNorthType.{DisperserNorth => Disperser, FPUnitNorth => FPU, FilterNorth => Filter, StageModeNorth => StageMode}

object GmosNorthEncoders extends GmosControllerEpics.Encoders[NorthTypes] {
  override val filter: EpicsCodex.EncodeEpicsValue[NorthTypes#Filter, (String, String)] = EncodeEpicsValue{
    case Filter.NONE                    => ("open1-6", "open2-8")
    case Filter.g_G0301                 => ("open1-6", "g_G0301")
    case Filter.r_G0303                 => ("open1-6", "r_G0303")
    case Filter.i_G0302                 => ("open1-6", "i_G0302")
    case Filter.z_G0304                 => ("open1-6", "z_G0304")
    case Filter.Z_G0322                 => ("Z_G0322", "open2-8")
    case Filter.Y_G0323                 => ("Y_G0323", "open2-8")
    case Filter.GG455_G0305             => ("GG455_G0305", "open2-8")
    case Filter.OG515_G0306             => ("OG515_G0306", "open2-8")
    case Filter.RG610_G0307             => ("RG610_G0307", "open2-8")
    case Filter.CaT_G0309               => ("CaT_G0309", "open2-8")
    case Filter.Ha_G0310                => ("open1-6", "Ha_G0310")
    case Filter.HaC_G0311               => ("open1-6", "HaC_G0311")
    case Filter.DS920_G0312             => ("open1-6", "DS920_G0312")
    case Filter.SII_G0317               => ("SII_G0317", "open2-8")
    case Filter.OIII_G0318              => ("OIII_G0318", "open2-8")
    case Filter.OIIIC_G0319             => ("OIIIC_G0319", "open2-8")
    case Filter.OVI_G0345               => ("open1-6", "OVI_G0345")
    case Filter.OVIC_G0346              => ("open1-6", "OVIC_G0346")
    case Filter.HeII_G0320              => ("open1-6", "HeII_G0320")
    case Filter.HeIIC_G0321             => ("open1-6", "HeIIC_G0321")
    case Filter.HartmannA_G0313_r_G0303 => ("HartmannA_G0313", "r_G0303")
    case Filter.HartmannB_G0314_r_G0303 => ("HartmannB_G0314", "r_G0303")
    case Filter.g_G0301_GG455_G0305     => ("GG455_G0305", "g_G0301")
    case Filter.g_G0301_OG515_G0306     => ("OG515_G0306", "g_G0301")
    case Filter.r_G0303_RG610_G0307     => ("RG610_G0307", "r_G0303")
    case Filter.i_G0302_CaT_G0309       => ("CaT_G0309", "i_G0302")
    case Filter.z_G0304_CaT_G0309       => ("CaT_G0309", "z_G0304")
    case Filter.u_G0308                 => ("open1-6", "open2-8")
    case Filter.ri_G0349                => ("open1-6", "ri_G0349")
  }

  override val fpu: EpicsCodex.EncodeEpicsValue[NorthTypes#FPU, (Option[String], Option[String])] = EncodeEpicsValue{
    a => {
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
        case FPU.NS_0        => ("NS0.25arcsec".some, Beam.InBeam.some)
        case FPU.NS_1        => ("NS0.5arcsec".some, Beam.InBeam.some)
        case FPU.NS_2        => ("NS0.75arcsec".some, Beam.InBeam.some)
        case FPU.NS_3        => ("NS1.0arcsec".some, Beam.InBeam.some)
        case FPU.NS_4        => ("NS1.5arcsec".some, Beam.InBeam.some)
        case FPU.NS_5        => ("NS2.0arcsec".some, Beam.InBeam.some)
        case FPU.CUSTOM_MASK => (none, none)
      }
      (r._1, r._2.map(GmosControllerEpics.beamEncoder.encode))
    }
  }

  override val stageMode: EpicsCodex.EncodeEpicsValue[NorthTypes#GmosStageMode, String] =  EncodeEpicsValue {
    case StageMode.NO_FOLLOW     => "MOVE"
    case StageMode.FOLLOW_XYZ    => "FOLLOW"
    case StageMode.FOLLOW_XY     => "FOLLOW-XY"
    case StageMode.FOLLOW_Z_ONLY => "FOLLOW-Z"
  }

  override val disperser: EpicsCodex.EncodeEpicsValue[NorthTypes#Disperser, String] = EncodeEpicsValue{
    case Disperser.MIRROR      => "mirror"
    case Disperser.B1200_G5301 => "B1200+_G5301"
    case Disperser.R831_G5302  => "R831+_G5302"
    case Disperser.B600_G5307  => "B600+_G5307"
    case Disperser.R600_G5304  => "R600+_G5304"
    case Disperser.R400_G5305  => "R400+_G5305"
    case Disperser.R150_G5308  => "R150+_G5308"
    case Disperser.B600_G5303  => "B600+_G5303"
    case Disperser.R150_G5306  => "R150+_G5306"
  }

  override val builtInROI: EncodeEpicsValue[BuiltinROI, Option[ROIValues]] = EncodeEpicsValue {
    case BuiltinROI.FULL_FRAME       => ROIValues.fromInt(xStart = 1, xSize = 6144, yStart = 1, ySize = 4224)
    case BuiltinROI.CCD2             => ROIValues.fromInt(xStart = 2049, xSize = 2048, yStart = 1, ySize = 4224)
    case BuiltinROI.CENTRAL_SPECTRUM => ROIValues.fromInt(xStart = 1, xSize = 6144, yStart = 1625, ySize = 1024)
    case BuiltinROI.CENTRAL_STAMP    => ROIValues.fromInt(xStart = 2923, xSize = 300, yStart = 1987, ySize = 308)
    case _                           => None
  }

  override val autoGain: EncodeEpicsValue[(GmosCommonType.AmpReadMode, GmosCommonType.AmpGain), Int] = {
    // gmosAutoGain.lut
    case (AmpReadMode.SLOW, AmpGain.LOW)  => 4
    case (AmpReadMode.SLOW, AmpGain.HIGH) => 0
    case (AmpReadMode.FAST, AmpGain.LOW)  => 15
    case (AmpReadMode.FAST, AmpGain.HIGH) => 3
  }

}

object GmosNorthControllerEpics {
  def apply[F[_]: Async: Timer: Logger](sys: => GmosEpics[F]): GmosController[F, NorthTypes] = {
    implicit val encoders = GmosNorthEncoders
    GmosControllerEpics[F, NorthTypes](sys, northConfigTypes)
  }
}
