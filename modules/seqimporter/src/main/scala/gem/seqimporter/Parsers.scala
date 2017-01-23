package gem.seqimporter

import edu.gemini.spModel.core.{Angle, OffsetP, OffsetQ}

import gem.Observation
import gem.enum._
import gem.config.GcalConfig.GcalLamp

import scalaz._
import Scalaz._

/** String parsers for our model types.
  */
object Parsers {

  import gem.seqimporter.pio.PioParse
  import gem.seqimporter.pio.PioParse._

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

  val obsId: PioParse[Observation.Id] =
    Observation.Id.fromString

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

      lamps => {
        val (continuum, arc) = ((List.empty[GcalContinuum], List.empty[GcalArc])/:lamps.split(',')) {
          case ((cs,as), s) =>
            val cs2 = lampToContinuum(s).fold(cs) { _ :: cs }
            val as2 = lampToArc(s).fold(as) { _ :: as }
            (cs2, as2)
        }
        GcalLamp.fromConfig(continuum.headOption, arc.strengthR(true): _*)
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
  }

  object Flamingos2 {

    import F2Disperser._
    val disperser: PioParse[F2Disperser] = enum(
      "NONE"    -> NoDisperser,
      "R1200HK" -> R1200HK,
      "R1200JH" -> R1200JH,
      "R3000"   -> R3000
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
      "Y"       -> Y
    )

    import F2FpUnit._
    val fpu: PioParse[F2FpUnit] = enum(
      "PINHOLE"        -> Pinhole,
      "SUBPIX_PINHOLE" -> SubPixPinhole,
      "FPU_NONE"       -> None,
      "CUSTOM_MASK"    -> Custom,
      "LONGSLIT_1"     -> LongSlit1,
      "LONGSLIT_2"     -> LongSlit2,
      "LONGSLIT_3"     -> LongSlit3,
      "LONGSLIT_4"     -> LongSlit4,
      "LONGSLIT_6"     -> LongSlit6,
      "LONGSLIT_8"     -> LongSlit8
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
}
