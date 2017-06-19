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

    val disperserTable = List(
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

    val filterTable = List(
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

    val fpuTable = List(
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
}
