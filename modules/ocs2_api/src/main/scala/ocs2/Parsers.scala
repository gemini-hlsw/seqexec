// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package ocs2

import cats.syntax.all._
import seqexec.model.Observation
import seqexec.model.Program
import lucuma.core.enum._
import ocs2.config.GcalConfig.GcalLamp
import lucuma.core.math._
import lucuma.core.optics.syntax.prism._

/** String parsers for our model types.
  */
object Parsers {

  import gem.ocs2.pio.PioParse
  import gem.ocs2.pio.PioParse._

  val yesNo: PioParse[Boolean] = enum(
    "No"  -> false,
    "Yes" -> true
  )

  val mosPreImaging: PioParse[MosPreImaging] = enum(
    "No"  -> MosPreImaging.IsNotMosPreImaging,
    "Yes" -> MosPreImaging.IsMosPreImaging
  )

  val arcsec: PioParse[Angle] =
    double.map(Angle.fromDoubleArcseconds)

  val angstroms: PioParse[Wavelength] =
    int.map(n => Wavelength.fromAngstrom(n).get)

  val magnitudeSystem: PioParse[MagnitudeSystem] =
    enumFromTag(MagnitudeSystem.all)

  val magnitudeBand: PioParse[MagnitudeBand] = enum(
    "u"  -> MagnitudeBand.SloanU,
    "g"  -> MagnitudeBand.SloanG,
    "r"  -> MagnitudeBand.SloanR,
    "i"  -> MagnitudeBand.SloanI,
    "z"  -> MagnitudeBand.SloanZ,
    "U"  -> MagnitudeBand.U,
    "B"  -> MagnitudeBand.B,
    "V"  -> MagnitudeBand.V,
    "UC" -> MagnitudeBand.Uc,
    "R"  -> MagnitudeBand.R,
    "I"  -> MagnitudeBand.I,
    "Y"  -> MagnitudeBand.Y,
    "J"  -> MagnitudeBand.J,
    "H"  -> MagnitudeBand.H,
    "K"  -> MagnitudeBand.K,
    "L"  -> MagnitudeBand.L,
    "M"  -> MagnitudeBand.M,
    "N"  -> MagnitudeBand.N,
    "Q"  -> MagnitudeBand.Q,
    "AP" -> MagnitudeBand.Ap
  )

  val ra: PioParse[RightAscension] =
    double
      .map(Angle.fromDoubleDegrees)
      .map(Angle.hourAngle.get)
      .map(RightAscension.fromHourAngle.get)

  val dec: PioParse[Declination] =
    double
      .map(Angle.fromDoubleDegrees)
      .map(Declination.fromAngle.unsafeGet)

  // Anything else blows up, which is ok since we don't support anything else
  val epoch: PioParse[Epoch] = enum(
    "1950.0" -> Epoch.B1950,
    "2000.0" -> Epoch.J2000
  )

  val ephemerisKeyType: PioParse[EphemerisKeyType] = enum(
    "asteroid"           -> EphemerisKeyType.AsteroidNew,
    "asteroid-old-style" -> EphemerisKeyType.AsteroidOld,
    "comet"              -> EphemerisKeyType.Comet,
    "major-body"         -> EphemerisKeyType.MajorBody
  )

  val instrument: PioParse[Instrument] = enum(
    "AcqCam"             -> lucuma.core.enum.Instrument.AcqCam,
    "bHROS"              -> lucuma.core.enum.Instrument.Bhros,
    "BHROS"              -> lucuma.core.enum.Instrument.Bhros,
    "Flamingos2"         -> lucuma.core.enum.Instrument.Flamingos2,
    "GMOS"               -> lucuma.core.enum.Instrument.GmosN,
    "GMOS-N"             -> lucuma.core.enum.Instrument.GmosN,
    "GMOSSouth"          -> lucuma.core.enum.Instrument.GmosS,
    "GMOS-S"             -> lucuma.core.enum.Instrument.GmosS,
    "GNIRS"              -> lucuma.core.enum.Instrument.Gnirs,
    "GPI"                -> lucuma.core.enum.Instrument.Gpi,
    "GSAOI"              -> lucuma.core.enum.Instrument.Gsaoi,
    "Michelle"           -> lucuma.core.enum.Instrument.Michelle,
    "NICI"               -> lucuma.core.enum.Instrument.Nici,
    "NIFS"               -> lucuma.core.enum.Instrument.Nifs,
    "NIRI"               -> lucuma.core.enum.Instrument.Niri,
    "Phoenix"            -> lucuma.core.enum.Instrument.Phoenix,
    "TReCS"              -> lucuma.core.enum.Instrument.Trecs,
    "Visitor"            -> lucuma.core.enum.Instrument.Visitor,
    "Visitor Instrument" -> lucuma.core.enum.Instrument.Visitor
  )

  val progId: PioParse[Program.Id] =
    PioParse(Program.Id.fromString.getOption)

  val obsId: PioParse[Observation.Id] =
    PioParse(Observation.Id.fromString)

  val datasetLabel: PioParse[Dataset.Label] =
    PioParse(Dataset.Label.fromString.getOption)

  val offsetP: PioParse[Offset.P] =
    arcsec.map(Offset.P.apply)

  val offsetQ: PioParse[Offset.Q] =
    arcsec.map(Offset.Q.apply)

  val userTargetType: PioParse[UserTargetType] = enum(
    "blindOffset" -> UserTargetType.BlindOffset,
    "offAxis"     -> UserTargetType.OffAxis,
    "tuningStar"  -> UserTargetType.TuningStar,
    "other"       -> UserTargetType.Other
  )

  val coadds: PioParse[CoAdds] = positiveShort.map(CoAdds.fromShort.unsafeGet)

  object Calibration {

    val lamp: PioParse[GcalLamp] = {
      import GcalArc._
      import GcalContinuum._

      val lampToContinuum = Map[String, GcalContinuum](
        "IR grey body - high" -> IrGreyBodyHigh,
        "IR grey body - low"  -> IrGreyBodyLow,
        "Quartz Halogen"      -> QuartzHalogen
      ).lift

      val lampToArc = Map[String, GcalArc](
        "Ar arc"   -> ArArc,
        "CuAr arc" -> CuArArc,
        "ThAr arc" -> ThArArc,
        "Xe arc"   -> XeArc
      ).lift

      PioParse { lamps =>
        val (continuum, arc) =
          lamps.split(',').foldLeft((List.empty[GcalContinuum], List.empty[GcalArc])) {
            case ((cs, as), s) =>
              val cs2 = lampToContinuum(s).fold(cs)(_ :: cs)
              val as2 = lampToArc(s).fold(as)(_ :: as)
              (cs2, as2)
          }
        GcalLamp.fromConfig(continuum.headOption, arc.tupleRight(true): _*)
      }
    }

    val filter: PioParse[GcalFilter] = enum(
      "none"         -> GcalFilter.None,
      "ND1.0"        -> GcalFilter.Nd10,
      "ND1.6"        -> GcalFilter.Nd16,
      "ND2.0"        -> GcalFilter.Nd20,
      "ND3.0"        -> GcalFilter.Nd30,
      "ND4.0"        -> GcalFilter.Nd40,
      "ND4-5"        -> GcalFilter.Nd45,
      "ND5.0"        -> GcalFilter.Nd50,
      "GMOS balance" -> GcalFilter.Gmos,
      "HROS balance" -> GcalFilter.Hros,
      "NIR balance"  -> GcalFilter.Nir
    )

    val diffuser: PioParse[GcalDiffuser] = enum(
      "IR"      -> GcalDiffuser.Ir,
      "visible" -> GcalDiffuser.Visible
    )

    val shutter: PioParse[GcalShutter] = enum(
      "Closed" -> GcalShutter.Closed,
      "Open"   -> GcalShutter.Open
    )

    val baseline: PioParse[GcalBaselineType] = enum(
      "DAY"   -> GcalBaselineType.Day,
      "NIGHT" -> GcalBaselineType.Night
    )
  }

  object Flamingos2 {

    import F2Disperser._

    val disperser: PioParse[Option[F2Disperser]] = enum(
      "NONE"    -> None,
      "R1200HK" -> Some(R1200HK),
      "R1200JH" -> Some(R1200JH),
      "R3000"   -> Some(R3000)
    )

    import F2Filter._

    val filter: PioParse[F2Filter] = enum(
      "OPEN"    -> Open,
      "DARK"    -> Dark,
      "F1056"   -> F1056,
      "F1063"   -> F1063,
      "H"       -> H,
      "HK"      -> HK,
      "J"       -> J,
      "J_LOW"   -> JLow,
      "JH"      -> JH,
      "K_LONG"  -> KLong,
      "K_SHORT" -> KShort,
      "K_BLUE"  -> KBlue,
      "K_RED"   -> KRed,
      "Y"       -> Y
    )

    import F2Fpu._
    import ocs2.config.F2Config.F2FpuChoice
    import ocs2.config.F2Config.F2FpuChoice.{ Builtin, Custom }

    val fpu: PioParse[Option[F2FpuChoice]] = enum(
      "PINHOLE"        -> Some(Builtin(Pinhole)),
      "SUBPIX_PINHOLE" -> Some(Builtin(SubPixPinhole)),
      "FPU_NONE"       -> None,
      "CUSTOM_MASK"    -> Some(Custom),
      "LONGSLIT_1"     -> Some(Builtin(LongSlit1)),
      "LONGSLIT_2"     -> Some(Builtin(LongSlit2)),
      "LONGSLIT_3"     -> Some(Builtin(LongSlit3)),
      "LONGSLIT_4"     -> Some(Builtin(LongSlit4)),
      "LONGSLIT_6"     -> Some(Builtin(LongSlit6)),
      "LONGSLIT_8"     -> Some(Builtin(LongSlit8))
    )

    import F2LyotWheel._

    val lyotWheel: PioParse[F2LyotWheel] = enum(
      "GEMS"       -> F33Gems,
      "GEMS_OVER"  -> GemsUnder,
      "GEMS_UNDER" -> GemsOver,
      "H1"         -> HartmannA,
      "H2"         -> HartmannB,
      "HIGH"       -> F32High,
      "LOW"        -> F32Low,
      "OPEN"       -> F16
    )

    import F2ReadMode._
    val readMode: PioParse[F2ReadMode] = enum(
      "BRIGHT_OBJECT_SPEC" -> Bright,
      "MEDIUM_OBJECT_SPEC" -> Medium,
      "FAINT_OBJECT_SPEC"  -> Faint
    )

    val windowCover: PioParse[F2WindowCover] = enum(
      "CLOSE" -> F2WindowCover.Close,
      "OPEN"  -> F2WindowCover.Open
    )
  }

  object Gmos {
    import GmosAdc._

    val adc: PioParse[Option[GmosAdc]] = enum(
      "No Correction"          -> Option.empty[GmosAdc],
      "Best Static Correction" -> Some(BestStatic),
      "Follow During Exposure" -> Some(Follow)
    )

    import GmosAmpCount._

    val ampCount: PioParse[GmosAmpCount] = enum(
      "Three"  -> Three,
      "Six"    -> Six,
      "Twelve" -> Twelve
    )

    import GmosAmpGain._

    val ampGain: PioParse[GmosAmpGain] = enum(
      "Low"  -> Low,
      "High" -> High
    )

    import GmosAmpReadMode._

    val ampReadMode: PioParse[GmosAmpReadMode] = enum(
      "Slow" -> Slow,
      "Fast" -> Fast
    )

    import GmosCustomSlitWidth._

    val customSlitWidth: PioParse[Option[GmosCustomSlitWidth]] = enum(
      "OTHER"             -> Option.empty[GmosCustomSlitWidth],
      "CUSTOM_WIDTH_0_25" -> Some(CustomWidth_0_25),
      "CUSTOM_WIDTH_0_50" -> Some(CustomWidth_0_50),
      "CUSTOM_WIDTH_0_75" -> Some(CustomWidth_0_75),
      "CUSTOM_WIDTH_1_00" -> Some(CustomWidth_1_00),
      "CUSTOM_WIDTH_1_50" -> Some(CustomWidth_1_50),
      "CUSTOM_WIDTH_2_00" -> Some(CustomWidth_2_00),
      "CUSTOM_WIDTH_5_00" -> Some(CustomWidth_5_00)
    )

    import GmosDetector._

    val detector: PioParse[GmosDetector] = enum(
      "E2V"       -> E2V,
      "HAMAMATSU" -> HAMAMATSU
    )

    val disperserOrder: PioParse[GmosDisperserOrder] = enum(
      "0" -> GmosDisperserOrder.Zero,
      "1" -> GmosDisperserOrder.One,
      "2" -> GmosDisperserOrder.Two
    )

    val disperserLambda: PioParse[Wavelength] =
      double.map(d => Wavelength.fromAngstrom((d * 10.0).round.toInt).get)

    val dtax: PioParse[GmosDtax] = enum(
      "-6" -> GmosDtax.MinusSix,
      "-5" -> GmosDtax.MinusFive,
      "-4" -> GmosDtax.MinusFour,
      "-3" -> GmosDtax.MinusThree,
      "-2" -> GmosDtax.MinusTwo,
      "-1" -> GmosDtax.MinusOne,
      "0"  -> GmosDtax.Zero,
      "1"  -> GmosDtax.One,
      "2"  -> GmosDtax.Two,
      "3"  -> GmosDtax.Three,
      "4"  -> GmosDtax.Four,
      "5"  -> GmosDtax.Five,
      "6"  -> GmosDtax.Six
    )

    val nsEOffsetting: PioParse[GmosEOffsetting] = enum(
      "true"  -> GmosEOffsetting.On,
      "false" -> GmosEOffsetting.Off
    )

    import ocs2.config.GmosConfig.GmosShuffleOffset

    val nsShuffle: PioParse[GmosShuffleOffset] =
      positiveInt.map(GmosShuffleOffset.unsafeFromRowCount)

    import ocs2.config.GmosConfig.GmosShuffleCycles

    val nsCycles: PioParse[GmosShuffleCycles] =
      positiveInt.map(GmosShuffleCycles.unsafeFromCycleCount)

    import GmosRoi._

    val roi: PioParse[GmosRoi] = enum(
      "Custom ROI"         -> Custom,
      "Full Frame Readout" -> FullFrame,
      "CCD 2"              -> Ccd2,
      "Central Spectrum"   -> CentralSpectrum,
      "Central Stamp"      -> CentralStamp,
      "Top Spectrum"       -> TopSpectrum,
      "Bottom Spectrum"    -> BottomSpectrum
    )

    val xBinning: PioParse[GmosXBinning] = enum(
      "1" -> GmosXBinning.One,
      "2" -> GmosXBinning.Two,
      "4" -> GmosXBinning.Four
    )

    val yBinning: PioParse[GmosYBinning] = enum(
      "1" -> GmosYBinning.One,
      "2" -> GmosYBinning.Two,
      "4" -> GmosYBinning.Four
    )
  }

  object GmosNorth {
    import GmosNorthDisperser._

    val disperser: PioParse[Option[GmosNorthDisperser]] = enum(
      "Mirror"      -> Option.empty[GmosNorthDisperser],
      "B1200_G5301" -> Some(B1200_G5301),
      "R831_G5302"  -> Some(R831_G5302),
      "B600_G5303"  -> Some(B600_G5303),
      "B600_G5307"  -> Some(B600_G5307),
      "R600_G5304"  -> Some(R600_G5304),
      "R400_G5305"  -> Some(R400_G5305),
      "R150_G5306"  -> Some(R150_G5306),
      "R150_G5308"  -> Some(R150_G5308)
    )

    import GmosNorthFilter._

    val filter: PioParse[Option[GmosNorthFilter]] = enum(
      "None"                      -> Option.empty[GmosNorthFilter],
      "g_G0301"                   -> Some(GPrime),
      "r_G0303"                   -> Some(RPrime),
      "i_G0302"                   -> Some(IPrime),
      "z_G0304"                   -> Some(ZPrime),
      "Z_G0322"                   -> Some(Z),
      "Y_G0323"                   -> Some(Y),
      "GG455_G0305"               -> Some(GG455),
      "OG515_G0306"               -> Some(OG515),
      "RG610_G0307"               -> Some(RG610),
      "CaT_G0309"                 -> Some(CaT),
      "Ha_G0310"                  -> Some(Ha),
      "HaC_G0311"                 -> Some(HaC),
      "DS920_G0312"               -> Some(DS920),
      "SII_G0317"                 -> Some(SII),
      "OIII_G0318"                -> Some(OIII),
      "OIIIC_G0319"               -> Some(OIIIC),
      "HeII_G0320"                -> Some(HeII),
      "HeIIC_G0321"               -> Some(HeIIC),
      "HartmannA_G0313 + r_G0303" -> Some(HartmannA_RPrime),
      "HartmannB_G0314 + r_G0303" -> Some(HartmannB_RPrime),
      "g_G0301 + GG455_G0305"     -> Some(GPrime_GG455),
      "g_G0301 + OG515_G0306"     -> Some(GPrime_OG515),
      "r_G0303 + RG610_G0307"     -> Some(RPrime_RG610),
      "i_G0302 + CaT_G0309"       -> Some(IPrime_CaT),
      "z_G0304 + CaT_G0309"       -> Some(ZPrime_CaT),
      "u_G0308"                   -> Some(UPrime)
    )

    import GmosNorthFpu._

    val fpu: PioParse[Option[GmosNorthFpu]] = enum(
      "None"                 -> Option.empty[GmosNorthFpu],
      "Longslit 0.25 arcsec" -> Some(LongSlit_0_25),
      "Longslit 0.50 arcsec" -> Some(LongSlit_0_50),
      "Longslit 0.75 arcsec" -> Some(LongSlit_0_75),
      "Longslit 1.00 arcsec" -> Some(LongSlit_1_00),
      "Longslit 1.50 arcsec" -> Some(LongSlit_1_50),
      "Longslit 2.00 arcsec" -> Some(LongSlit_2_00),
      "Longslit 5.00 arcsec" -> Some(LongSlit_5_00),
      "IFU 2 Slits"          -> Some(Ifu1),
      "IFU Left Slit (blue)" -> Some(Ifu2),
      "IFU Right Slit (red)" -> Some(Ifu3),
      "N and S 0.25 arcsec"  -> Some(Ns0),
      "N and S 0.50 arcsec"  -> Some(Ns1),
      "N and S 0.75 arcsec"  -> Some(Ns2),
      "N and S 1.00 arcsec"  -> Some(Ns3),
      "N and S 1.50 arcsec"  -> Some(Ns4),
      "N and S 2.00 arcsec"  -> Some(Ns5),
      "Custom Mask"          -> Option.empty[GmosNorthFpu]
    )

    import GmosNorthStageMode._

    val stageMode: PioParse[GmosNorthStageMode] = enum(
      "Do Not Follow"        -> NoFollow,
      "Follow in XYZ(focus)" -> FollowXyz,
      "Follow in XY"         -> FollowXy,
      "Follow in Z Only"     -> FollowZ
    )
  }

  object GmosSouth {
    import GmosSouthDisperser._

    val disperser: PioParse[Option[GmosSouthDisperser]] = enum(
      "Mirror"      -> Option.empty[GmosSouthDisperser],
      "B1200_G5321" -> Some(B1200_G5321),
      "R831_G5322"  -> Some(R831_G5322),
      "B600_G5323"  -> Some(B600_G5323),
      "R600_G5324"  -> Some(R600_G5324),
      "R400_G5325"  -> Some(R400_G5325),
      "R150_G5326"  -> Some(R150_G5326)
    )

    import GmosSouthFilter._

    val filter: PioParse[Option[GmosSouthFilter]] = enum(
      "None"                      -> Option.empty[GmosSouthFilter],
      "u_G0332"                   -> Some(UPrime),
      "g_G0325"                   -> Some(GPrime),
      "r_G0326"                   -> Some(RPrime),
      "i_G0327"                   -> Some(IPrime),
      "z_G0328"                   -> Some(ZPrime),
      "Z_G0343"                   -> Some(Z),
      "Y_G0344"                   -> Some(Y),
      "GG455_G0329"               -> Some(GG455),
      "OG515_G0330"               -> Some(OG515),
      "RG610_G0331"               -> Some(RG610),
      "RG780_G0334"               -> Some(RG780),
      "CaT_G0333"                 -> Some(CaT),
      "HartmannA_G0337 + r_G0326" -> Some(HartmannA_RPrime),
      "HartmannB_G0338 + r_G0326" -> Some(HartmannB_RPrime),
      "g_G0325 + GG455_G0329"     -> Some(GPrime_GG455),
      "g_G0325 + OG515_G0330"     -> Some(GPrime_OG515),
      "r_G0326 + RG610_G0331"     -> Some(RPrime_RG610),
      "i_G0327 + RG780_G0334"     -> Some(IPrime_RG780),
      "i_G0327 + CaT_G0333"       -> Some(IPrime_CaT),
      "z_G0328 + CaT_G0333"       -> Some(ZPrime_CaT),
      "Ha_G0336"                  -> Some(Ha),
      "SII_G0335"                 -> Some(SII),
      "HaC_G0337"                 -> Some(HaC),
      "OIII_G0338"                -> Some(OIII),
      "OIIIC_G0339"               -> Some(OIIIC),
      "HeII_G0340"                -> Some(HeII),
      "HeIIC_G0341"               -> Some(HeIIC),
      "Lya395_G0342"              -> Some(Lya395)
    )

    import GmosSouthFpu._

    val fpu: PioParse[Option[GmosSouthFpu]] = enum(
      "None"                         -> Option.empty[GmosSouthFpu],
      "Longslit 0.25 arcsec"         -> Some(LongSlit_0_25),
      "Longslit 0.50 arcsec"         -> Some(LongSlit_0_50),
      "Longslit 0.75 arcsec"         -> Some(LongSlit_0_75),
      "Longslit 1.00 arcsec"         -> Some(LongSlit_1_00),
      "Longslit 1.50 arcsec"         -> Some(LongSlit_1_50),
      "Longslit 2.00 arcsec"         -> Some(LongSlit_2_00),
      "Longslit 5.00 arcsec"         -> Some(LongSlit_5_00),
      "IFU 2 Slits"                  -> Some(Ifu1),
      "IFU Left Slit (blue)"         -> Some(Ifu2),
      "IFU Right Slit (red)"         -> Some(Ifu3),
      "bHROS"                        -> Some(Bhros),
      "IFU N and S 2 Slits"          -> Some(IfuN),
      "IFU N and S Left Slit (blue)" -> Some(IfuNB),
      "IFU N and S Right Slit (red)" -> Some(IfuNR),
      "N and S 0.50 arcsec"          -> Some(Ns1),
      "N and S 0.75 arcsec"          -> Some(Ns2),
      "N and S 1.00 arcsec"          -> Some(Ns3),
      "N and S 1.50 arcsec"          -> Some(Ns4),
      "N and S 2.00 arcsec"          -> Some(Ns5),
      "Custom Mask"                  -> Option.empty[GmosSouthFpu]
    )

    import GmosSouthStageMode._

    val stageMode: PioParse[GmosSouthStageMode] = enum(
      "Do Not Follow"        -> NoFollow,
      "Follow in XYZ(focus)" -> FollowXyz,
      "Follow in XY"         -> FollowXy,
      "Follow in Z Only"     -> FollowZ
    )
  }

  object Gnirs {

    val acquisitionMirror: PioParse[GnirsAcquisitionMirror] = {

      import GnirsAcquisitionMirror._

      enum(
        "in"  -> In,
        "out" -> Out
      )
    }

    val camera: PioParse[GnirsCamera] = {

      import GnirsCamera._

      enum(
        "short blue" -> ShortBlue,
        "long blue"  -> LongBlue,
        "short red"  -> ShortRed,
        "long red"   -> LongRed
      )
    }

    val decker: PioParse[GnirsDecker] = {

      import GnirsDecker._

      enum(
        "acquisition"            -> Acquisition,
        "pupil viewer"           -> PupilViewer,
        "short camera long slit" -> ShortCamLongSlit,
        "short camera x-disp"    -> ShortCamCrossDispersed,
        "long camera x-disp"     -> LongCamCrossDispersed,
        "IFU"                    -> Ifu,
        "long camera long slit"  -> LongCamLongSlit,
        "wollaston"              -> Wollaston
      )

    }

    val disperser: PioParse[GnirsDisperser] = {

      import GnirsDisperser._

      enum(
        "10 l/mm grating"  -> D10,
        "32 l/mm grating"  -> D32,
        "111 l/mm grating" -> D111
      )

    }

    val filter: PioParse[GnirsFilter] = {

      import GnirsFilter._

      enum(
        "x-dispersed" -> CrossDispersed,
        "order 6 (X)" -> Order6,
        "order 5 (J)" -> Order5,
        "order 4 (H)" -> Order4,
        "order 3 (K)" -> Order3,
        "order 2 (L)" -> Order2,
        "order 1 (M)" -> Order1,
        "H2: 2.12um"  -> H2,
        "H + ND100X"  -> HNd100x,
        "H2 + ND100X" -> H2Nd100x,
        "PAH: 3.3um"  -> PAH,
        "Y: 1.03um"   -> Y,
        "J: 1.25um"   -> J,
        "K: 2.20um"   -> K
      )

    }

    val fpu: PioParse[Either[GnirsFpuOther, GnirsFpuSlit]] = {

      import GnirsFpuSlit._
      import GnirsFpuOther._

      enum(
        "0.10 arcsec"  -> Right(LongSlit_0_10),
        "0.15 arcsec"  -> Right(LongSlit_0_15),
        "0.20 arcsec"  -> Right(LongSlit_0_20),
        "0.30 arcsec"  -> Right(LongSlit_0_30),
        "0.45 arcsec"  -> Right(LongSlit_0_45),
        "0.675 arcsec" -> Right(LongSlit_0_675),
        "1.0 arcsec"   -> Right(LongSlit_1_00),
        "3.0 arcsec"   -> Right(LongSlit_3_00),
        "IFU"          -> Left(Ifu),
        "acquisition"  -> Left(Acquisition),
        "pupil viewer" -> Left(PupilViewer),
        "pinhole 0.1"  -> Left(Pinhole1),
        "pinhole 0.3"  -> Left(Pinhole3)
      )

    }

    val prism: PioParse[GnirsPrism] = {

      import GnirsPrism._

      enum(
        "No"  -> Mirror,
        "SXD" -> Sxd,
        "LXD" -> Lxd
      )

    }

    val readMode: PioParse[GnirsReadMode] = {

      import GnirsReadMode._

      enum(
        "Very Bright/Acq./High Bckgrd." -> VeryBright,
        "Bright Objects"                -> Bright,
        "Faint Objects"                 -> Faint,
        "Very Faint Objects"            -> VeryFaint
      )

    }

    val centralWavelength: PioParse[Wavelength] =
      bigDecimal.map(w =>
        Wavelength
          .fromAngstrom(
            w.underlying.movePointRight(4).intValue
          )
          .get
      )

    val wellDepth: PioParse[GnirsWellDepth] = {

      import GnirsWellDepth._

      enum(
        "SHALLOW" -> Shallow,
        "DEEP"    -> Deep
      )

    }

    object SmartGcal {

      val mode: PioParse[GnirsAcquisitionMirror] = {

        import GnirsAcquisitionMirror._

        enum(
          "IMAGING"      -> In,
          "SPECTROSCOPY" -> Out
        )

      }

      val pixelScale: PioParse[GnirsPixelScale] = {

        import GnirsPixelScale._

        enum(
          "0.05\"/pix" -> PixelScale_0_05,
          "0.15\"/pix" -> PixelScale_0_15
        )

      }

    }

  }

  object Gpi {

    val observingMode: PioParse[GpiObservingMode] = enum(
      GpiObservingMode.all.fproduct(_.longName).map(_.swap): _*
    )

  }
}
