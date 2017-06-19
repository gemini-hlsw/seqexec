// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import edu.gemini.spModel.core.{Angle, OffsetP, OffsetQ, ProgramId}

import gem.{Dataset, Observation, Program}
import gem.enum._
import gem.config.GcalConfig.GcalLamp

import scalaz._
import Scalaz._

/** String parsers for our model types.
  */
object Parsers {

  import gem.ocs2.pio.PioParse
  import gem.ocs2.pio.PioParse._

  val yesNo: PioParse[Boolean] = enum(
    "No"  -> false,
    "Yes" -> true
  )

  val arcsec: PioParse[Angle] =
    double.map(Angle.fromArcsecs)

  val instrument: PioParse[Instrument] = enum(
    "AcqCam"     -> gem.enum.Instrument.AcqCam,
    "bHROS"      -> gem.enum.Instrument.Bhros,
    "BHROS"      -> gem.enum.Instrument.Bhros,
    "Flamingos2" -> gem.enum.Instrument.Flamingos2,
    "GMOS"       -> gem.enum.Instrument.GmosN,
    "GMOS-N"     -> gem.enum.Instrument.GmosN,
    "GMOSSouth"  -> gem.enum.Instrument.GmosS,
    "GMOS-S"     -> gem.enum.Instrument.GmosS,
    "GNIRS"      -> gem.enum.Instrument.Gnirs,
    "GPI"        -> gem.enum.Instrument.Gpi,
    "GSAOI"      -> gem.enum.Instrument.Gsaoi,
    "Michelle"   -> gem.enum.Instrument.Michelle,
    "NICI"       -> gem.enum.Instrument.Nici,
    "NIFS"       -> gem.enum.Instrument.Nifs,
    "NIRI"       -> gem.enum.Instrument.Niri,
    "Phoenix"    -> gem.enum.Instrument.Phoenix,
    "TReCS"      -> gem.enum.Instrument.Trecs,
    "Visitor"    -> gem.enum.Instrument.Visitor,
    "Visitor Instrument" -> gem.enum.Instrument.Visitor
  )

  val progId: PioParse[Program.Id] =
    PioParse(s => Option(ProgramId.parse(s)))

  val obsId: PioParse[Observation.Id] =
    PioParse(Observation.Id.fromString)

  val datasetLabel: PioParse[Dataset.Label] =
    PioParse(Dataset.Label.fromString)

  val offsetP: PioParse[OffsetP] =
    arcsec.map(OffsetP.apply)

  val offsetQ: PioParse[OffsetQ] =
    arcsec.map(OffsetQ.apply)

  object Calibration {

    val lamp: PioParse[GcalLamp] = {
      import GcalArc._
      import GcalContinuum._

      val lampToContinuum = Map[String, GcalContinuum](
        "IR grey body - high" -> IrGreyBodyHigh,
        "IR grey body - low"  -> IrGreyBodyLow,
        "Quartz Halogen"      -> QuartzHalogen
      ).lift

      val lampToArc       = Map[String, GcalArc](
        "Ar arc"              -> ArArc,
        "CuAr arc"            -> CuArArc,
        "ThAr Arc"            -> ThArArc,
        "Xe Arc"              -> XeArc
      ).lift

      PioParse(lamps => {
        val (continuum, arc) = ((List.empty[GcalContinuum], List.empty[GcalArc])/:lamps.split(',')) {
          case ((cs,as), s) =>
            val cs2 = lampToContinuum(s).fold(cs) { _ :: cs }
            val as2 = lampToArc(s).fold(as) { _ :: as }
            (cs2, as2)
        }
        GcalLamp.fromConfig(continuum.headOption, arc.strengthR(true): _*)
      })
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
  }

  private def fstParser[A](table: List[(String, String, A)]): PioParse[A] =
    enum(table.map { case (a, _, b) => (a, b) }:_*)

  private def sndParser[A](table: List[(String, String, A)]): PioParse[A] =
    enum(table.map { case (_, a, b) => (a, b) }:_*)

  object Flamingos2 {

    import F2Disperser._

    val disperserTable: List[(String, String, F2Disperser)] =
      List(
        ("NONE",    "None",                       NoDisperser),
        ("R1200HK", "R=1200 (H + K) grism",       R1200HK    ),
        ("R1200JH", "R=1200 (J + H) grism",       R1200JH    ),
        ("R3000",   "R=3000 (J or H or K) grism", R3000      )
      )

    val disperser: PioParse[F2Disperser] =
      fstParser(disperserTable)

    val disperserDisplayValue: PioParse[F2Disperser] =
      sndParser(disperserTable)

    import F2Filter._

    val filterTable: List[(String, String, F2Filter)] =
      List(
        ("OPEN",    "Open",               Open  ),
        ("DARK",    "Dark",               Dark  ),
        ("F1056",   "F1056 (1.056 um)",   F1056 ),
        ("F1063",   "F1063 (1.063 um)",   F1063 ),
        ("H",       "H (1.65 um)",        H     ),
        ("HK",      "HK (spectroscopic)", HK    ),
        ("J",       "J (1.25 um)",        J     ),
        ("J_LOW",   "J-low (1.15 um)",    JLow  ),
        ("JH",      "JH (spectroscopic)", JH    ),
        ("K_LONG",  "K-long (2.20 um)",   KLong ),
        ("K_SHORT", "K-short (2.15 um)",  KShort),
        ("K_BLUE",  "K-blue (2.06 um)",   KBlue ),
        ("K_RED",   "K-red (2.31 um)",    KRed  ),
        ("Y",       "Y (1.02 um)",        Y     )
      )

    val filter: PioParse[F2Filter] =
      fstParser(filterTable)

    val filterDisplayValue: PioParse[F2Filter] =
      sndParser(filterTable)

    import F2FpUnit._

    val fpuTable: List[(String, String, F2FpUnit)] =
      List(
        ("PINHOLE",        "2-pix pinhole grid",  Pinhole      ),
        ("SUBPIX_PINHOLE", "subpix pinhole grid", SubPixPinhole),
        ("FPU_NONE",       "Imaging (none)",      None         ),
        ("CUSTOM_MASK",    "Custom Mask",         Custom       ),
        ("LONGSLIT_1",     "1-pix longslit",      LongSlit1    ),
        ("LONGSLIT_2",     "2-pix longslit",      LongSlit2    ),
        ("LONGSLIT_3",     "3-pix longslit",      LongSlit3    ),
        ("LONGSLIT_4",     "4-pix longslit",      LongSlit4    ),
        ("LONGSLIT_6",     "6-pix longslit",      LongSlit6    ),
        ("LONGSLIT_8",     "8-pix longslit",      LongSlit8    )
      )

    val fpu: PioParse[F2FpUnit] =
      fstParser(fpuTable)

    val fpuDisplayValue: PioParse[F2FpUnit] =
      sndParser(fpuTable)

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

  object GmosNorth {
    import GmosNorthDisperser._

    val disperserTable = List(
      ("MIRROR",      "Mirror",      Option.empty[GmosNorthDisperser]),
      ("B1200_G5301", "B1200_G5301", Some(B1200_G5301)               ),
      ("R831_G5302",  "R831_G5302",  Some(R831_G5302)                ),
      ("B600_G5303",  "B600_G5303",  Some(B600_G5303)                ),
      ("B600_G5307",  "B600_G5307",  Some(B600_G5307)                ),
      ("R600_G5304",  "R600_G5304",  Some(R600_G5304)                ),
      ("R400_G5305",  "R400_G5305",  Some(R400_G5305)                ),
      ("R150_G5306",  "R150_G5306",  Some(R150_G5306)                ),
      ("R150_G5308",  "R150_G5308",  Some(R150_G5308)                )
    )

    val disperser: PioParse[Option[GmosNorthDisperser]] =
      fstParser(disperserTable)

    val disperserDisplayValue: PioParse[Option[GmosNorthDisperser]] =
      sndParser(disperserTable)

    import GmosNorthFilter._

    val filterTable = List(
      ("NONE",                    "None",                      Option.empty[GmosNorthFilter]),
      ("g_G0301",                 "g_G0301",                   Some(GPrime)                 ),
      ("r_G0303",                 "r_G0303",                   Some(RPrime)                 ),
      ("i_G0302",                 "i_G0302",                   Some(IPrime)                 ),
      ("z_G0304",                 "z_G0304",                   Some(ZPrime)                 ),
      ("Z_G0322",                 "Z_G0322",                   Some(Z)                      ),
      ("Y_G0323",                 "Y_G0323",                   Some(Y)                      ),
      ("GG455_G0305",             "GG455_G0305",               Some(GG455)                  ),
      ("OG515_G0306",             "OG515_G0306",               Some(OG515)                  ),
      ("RG610_G0307",             "RG610_G0307",               Some(RG610)                  ),
      ("CaT_G0309",               "CaT_G0309",                 Some(CaT)                    ),
      ("Ha_G0310",                "Ha_G0310",                  Some(Ha)                     ),
      ("HaC_G0311",               "HaC_G0311",                 Some(HaC)                    ),
      ("DS920_G0312",             "DS920_G0312",               Some(DS920)                  ),
      ("SII_G0317",               "SII_G0317",                 Some(SII)                    ),
      ("OIII_G0318",              "OIII_G0318",                Some(OIII)                   ),
      ("OIIIC_G0319",             "OIIIC_G0319",               Some(OIIIC)                  ),
      ("HeII_G0320",              "HeII_G0320",                Some(HeII)                   ),
      ("HeIIC_G0321",             "HeIIC_G0321",               Some(HeIIC)                  ),
      ("HartmannA_G0313_r_G0303", "HartmannA_G0313 + r_G0303", Some(HartmannA_RPrime)       ),
      ("HartmannB_G0314_r_G0303", "HartmannB_G0314 + r_G0303", Some(HartmannB_RPrime)       ),
      ("g_G0301_GG455_G0305",     "g_G0301 + GG455_G0305",     Some(GPrime_GG455)           ),
      ("g_G0301_OG515_G0306",     "g_G0301 + OG515_G0306",     Some(GPrime_OG515)           ),
      ("r_G0303_RG610_G0307",     "r_G0303 + RG610_G0307",     Some(RPrime_RG610)           ),
      ("i_G0302_CaT_G0309",       "i_G0302 + CaT_G0309",       Some(IPrime_CaT)             ),
      ("z_G0304_CaT_G0309",       "z_G0304 + CaT_G0309",       Some(ZPrime_CaT)             ),
      ("u_G0308",                 "u_G0308",                   Some(UPrime)                 )
    )

    val filter: PioParse[Option[GmosNorthFilter]] =
      fstParser(filterTable)

    val filterDisplayValue: PioParse[Option[GmosNorthFilter]] =
      sndParser(filterTable)


    import GmosNorthFpu._

    val fpuTable = List(
      ("FPU_NONE",   "None",                 Option.empty[GmosNorthFpu]),
      ("LONGSLIT_1", "Longslit 0.25 arcsec", Some(Longslit1)           ),
      ("LONGSLIT_2", "Longslit 0.50 arcsec", Some(Longslit2)           ),
      ("LONGSLIT_3", "Longslit 0.75 arcsec", Some(Longslit3)           ),
      ("LONGSLIT_4", "Longslit 1.00 arcsec", Some(Longslit4)           ),
      ("LONGSLIT_5", "Longslit 1.50 arcsec", Some(Longslit5)           ),
      ("LONGSLIT_6", "Longslit 2.00 arcsec", Some(Longslit6)           ),
      ("LONGSLIT_7", "Longslit 5.00 arcsec", Some(Longslit7)           ),
      ("IFU_1",      "IFU 2 Slits",          Some(Ifu1)                ),
      ("IFU_2",      "IFU Left Slit (blue)", Some(Ifu2)                ),
      ("IFU_3",      "IFU Right Slit (red)", Some(Ifu3)                ),
      ("NS_0",       "N and S 0.25 arcsec",  Some(Ns0)                 ),
      ("NS_1",       "N and S 0.50 arcsec",  Some(Ns1)                 ),
      ("NS_2",       "N and S 0.75 arcsec",  Some(Ns2)                 ),
      ("NS_3",       "N and S 1.00 arcsec",  Some(Ns3)                 ),
      ("NS_4",       "N and S 1.50 arcsec",  Some(Ns4)                 ),
      ("NS_5",       "N and S 2.00 arcsec",  Some(Ns5)                 )
    )

    val fpu: PioParse[Option[GmosNorthFpu]] =
      fstParser(fpuTable)

    val fpuDisplayValue: PioParse[Option[GmosNorthFpu]] =
      sndParser(fpuTable)


    import GmosNorthStageMode._

    val stageMode: PioParse[GmosNorthStageMode] = enum(
      "NO_FOLLOW"     -> NoFollow,
      "FOLLOW_XYZ"    -> FollowXyz,
      "FOLLOW_XY"     -> FollowXy,
      "FOLLOW_Z_ONLY" -> FollowZ
    )
  }

  object GmosSouth {
    import GmosSouthDisperser._

    val disperserTable = List(
      ("MIRROR",      "Mirror",      Option.empty[GmosSouthDisperser]),
      ("B1200_G5321", "B1200_G5321", Some(B1200_G5321)               ),
      ("R831_G5322",  "R831_G5322",  Some(R831_G5322)                ),
      ("B600_G5323",  "B600_G5323",  Some(B600_G5323)                ),
      ("R600_G5324",  "R600_G5324",  Some(R600_G5324)                ),
      ("R400_G5325",  "R400_G5325",  Some(R400_G5325)                ),
      ("R150_G5326",  "R150_G5326",  Some(R150_G5326)                )
    )

    val disperser: PioParse[Option[GmosSouthDisperser]] =
      fstParser(disperserTable)

    val disperserDisplayValue: PioParse[Option[GmosSouthDisperser]] =
      sndParser(disperserTable)

    import GmosSouthFilter._

    val filterTable = List(
      ("NONE",                    "None",                      Option.empty[GmosSouthFilter]),
      ("u_G0332",                 "u_G0332",                   Some(UPrime)                 ),
      ("g_G0325",                 "g_G0325",                   Some(GPrime)                 ),
      ("r_G0326",                 "r_G0326",                   Some(RPrime)                 ),
      ("i_G0327",                 "i_G0327",                   Some(IPrime)                 ),
      ("z_G0328",                 "z_G0328",                   Some(ZPrime)                 ),
      ("Z_G0343",                 "Z_G0343",                   Some(Z)                      ),
      ("Y_G0344",                 "Y_G0344",                   Some(Y)                      ),
      ("GG455_G0329",             "GG455_G0329",               Some(GG455)                  ),
      ("OG515_G0330",             "OG515_G0330",               Some(OG515)                  ),
      ("RG610_G0331",             "RG610_G0331",               Some(RG610)                  ),
      ("RG780_G0334",             "RG780_G0334",               Some(RG780)                  ),
      ("CaT_G0333",               "CaT_G0333",                 Some(CaT)                    ),
      ("HartmannA_G0337_r_G0326", "HartmannA_G0337 + r_G0326", Some(HartmannA_RPrime)       ),
      ("HartmannB_G0338_r_G0326", "HartmannB_G0338 + r_G0326", Some(HartmannB_RPrime)       ),
      ("g_G0325_GG455_G0329",     "g_G0325 + GG455_G0329",     Some(GPrime_GG455)           ),
      ("g_G0325_OG515_G0330",     "g_G0325 + OG515_G0330",     Some(GPrime_OG515)           ),
      ("r_G0326_RG610_G0331",     "r_G0326 + RG610_G0331",     Some(RPrime_RG610)           ),
      ("i_G0327_RG780_G0334",     "i_G0327 + RG780_G0334",     Some(IPrime_RG780)           ),
      ("i_G0327_CaT_G0333",       "i_G0327 + CaT_G0333",       Some(IPrime_CaT)             ),
      ("z_G0328_CaT_G0333",       "z_G0328 + CaT_G0333",       Some(ZPrime_CaT)             ),
      ("Ha_G0336",                "Ha_G0336",                  Some(Ha)                     ),
      ("SII_G0335",               "SII_G0335",                 Some(SII)                    ),
      ("HaC_G0337",               "HaC_G0337",                 Some(HaC)                    ),
      ("OIII_G0338",              "OIII_G0338",                Some(OIII)                   ),
      ("OIIIC_G0339",             "OIIIC_G0339",               Some(OIIIC)                  ),
      ("HeII_G0340",              "HeII_G0340",                Some(HeII)                   ),
      ("HeIIC_G0341",             "HeIIC_G0341",               Some(HeIIC)                  ),
      ("Lya395_G0342",            "Lya395_G0342",              Some(Lya395)                 )
    )

    val filter: PioParse[Option[GmosSouthFilter]] =
      fstParser(filterTable)

    val filterDisplayValue: PioParse[Option[GmosSouthFilter]] =
      sndParser(filterTable)


    import GmosSouthFpu._

    val fpuTable = List(
      ("FPU_NONE",   "None",                         Option.empty[GmosSouthFpu]),
      ("LONGSLIT_1", "Longslit 0.25 arcsec",         Some(Longslit1)           ),
      ("LONGSLIT_2", "Longslit 0.50 arcsec",         Some(Longslit2)           ),
      ("LONGSLIT_3", "Longslit 0.75 arcsec",         Some(Longslit3)           ),
      ("LONGSLIT_4", "Longslit 1.00 arcsec",         Some(Longslit4)           ),
      ("LONGSLIT_5", "Longslit 1.50 arcsec",         Some(Longslit5)           ),
      ("LONGSLIT_6", "Longslit 2.00 arcsec",         Some(Longslit6)           ),
      ("LONGSLIT_7", "Longslit 5.00 arcsec",         Some(Longslit7)           ),
      ("IFU_1",      "IFU 2 Slits",                  Some(Ifu1)                ),
      ("IFU_2",      "IFU Left Slit (blue)",         Some(Ifu2)                ),
      ("IFU_3",      "IFU Right Slit (red)",         Some(Ifu3)                ),
      ("BHROS",      "bHROS",                        Some(Bhros)               ),
      ("IFU_N",      "IFU N and S 2 Slits",          Some(IfuN)                ),
      ("IFU_N_B",    "IFU N and S Left Slit (blue)", Some(IfuNB)               ),
      ("IFU_N_R",    "IFU N and S Right Slit (red)", Some(IfuNR)               ),
      ("NS_1",       "N and S 0.50 arcsec",          Some(Ns1)                 ),
      ("NS_2",       "N and S 0.75 arcsec",          Some(Ns2)                 ),
      ("NS_3",       "N and S 1.00 arcsec",          Some(Ns3)                 ),
      ("NS_4",       "N and S 1.50 arcsec",          Some(Ns4)                 ),
      ("NS_5",       "N and S 2.00 arcsec",          Some(Ns5)                 )
    )

    val fpu: PioParse[Option[GmosSouthFpu]] =
      fstParser(fpuTable)

    val fpuDisplayValue: PioParse[Option[GmosSouthFpu]] =
      sndParser(fpuTable)


    import GmosSouthStageMode._

    val stageMode: PioParse[GmosSouthStageMode] = enum(
      "NO_FOLLOW"     -> NoFollow,
      "FOLLOW_XYZ"    -> FollowXyz,
      "FOLLOW_XY"     -> FollowXy,
      "FOLLOW_Z_ONLY" -> FollowZ
    )
  }
}
