package gem.seqimporter

import edu.gemini.spModel.core._
import gem._
import gem.config._
import gem.enum._

import java.time.Duration

import scala.reflect.runtime.universe.TypeTag
import scalaz._
import Scalaz._

object ConfigReader {
  type Read[A] = String => A

  // Clean up classnames in toString, which tells you the type of the key, which should be
  // helpful for debugging.
  private def clean(s: String) =
    s.replace("edu.gemini.spModel.core.", "")
     .replace("java.lang.", "")
     .replace("java.time.", "")
     .replace("gem.", "")

  object Read {
    val int:     Read[Int     ] = _.toInt
    val long:    Read[Long    ] = _.toLong
    val double:  Read[Double  ] = _.toDouble
    val boolean: Read[Boolean ] = _.toBoolean
    val yesNo:   Read[Boolean ] = {
      case "No"  => false
      case "Yes" => true
      case x     => sys.error(s"unrecognized Yes/No value: $x")
    }
    def arcsec:  Read[Angle   ] = double andThen Angle.fromArcsecs
    val durSecs: Read[Duration] = double andThen { d => Duration.ofMillis((d * 1000).round) }
  }

  sealed abstract class System(val system: String) {

    final class Key[A](val name: String, val read: Read[A])(implicit ev: TypeTag[A]) {
      val path = s"$system:$name"

      def legacyGet(cm: ConfigMap): Option[String] =
        cm.lookup(path)

      override def toString: String =
        s"Key[${clean(ev.tpe.toString)}]($path)"
    }

    protected object Key {
      def apply[A: TypeTag](name: String)(read: Read[A]): Key[A] =
        new Key(name, read)

      def enum[A: TypeTag](name: String, m: (String, A)*): Key[A] =
        enumWithDefault[A](name, s => sys.error(s"new value of '$name' for $s not found"), m: _*)

      def enumWithDefault[A: TypeTag](name: String, default: String => A, m: (String, A)*): Key[A] =
        new Key[A](name, m.toMap.withDefault(default))
    }
  }

  object Legacy {
    case object Telescope extends System("telescope") {
      val P = Key[OffsetP]("p")(Read.arcsec andThen OffsetP.apply)
      val Q = Key[OffsetQ]("q")(Read.arcsec andThen OffsetQ.apply)
    }

    case object Observe extends System("observe") {
      val ObserveType  = Key[String  ]("observeType" )(identity    )
      val ExposureTime = Key[Duration]("exposureTime")(Read.durSecs)
      val Status       = Key[Boolean ]("status"      ) {
        case "complete" => true
        case _          => false
      }
    }

    case object Ocs extends System("ocs") {
      val ObservationId = Key[Observation.Id]("observationId")(Observation.Id.unsafeFromString)
    }

    case object Instrument extends System("instrument") {
      val Instrument    = Key.enum[Instrument]("instrument",
        "Flamingos2" -> gem.enum.Instrument.Flamingos2
      )
      val MosPreImaging = Key[Boolean]("mosPreimaging")(Read.yesNo)

      // Instruments share config fields (all filters go into "instrument:filter"
      // for example) so we repeat them below, name-spaced per instrument for
      // type safety.

      object F2 {
        import F2Disperser._
        val Disperser = Key.enum[F2Disperser]("disperser",
          "NONE"    -> NoDisperser,
          "R1200HK" -> R1200HK,
          "R1200JH" -> R1200JH,
          "R3000"   -> R3000
        )

        import F2Filter._
        val Filter = Key.enum[F2Filter]("filter",
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
        val Fpu = Key.enum[F2FpUnit]("fpu",
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
        val LyotWheel   = Key.enum[F2LyotWheel]("lyotWheel",
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
        val ReadMode    = Key.enum[F2ReadMode]("readMode",
          "BRIGHT_OBJECT_SPEC" -> Bright,
          "MEDIUM_OBJECT_SPEC" -> Medium,
          "FAINT_OBJECT_SPEC"  -> Faint
        )

        val WindowCover = Key.enum[F2WindowCover]("windowCover",
          "CLOSE" -> F2WindowCover.Close,
          "OPEN"  -> F2WindowCover.Open
        )
      }
    }

    case object Calibration extends System("calibration") {

      import GcalArc._
      import GcalContinuum._
      import gem.config.GcalConfig.GcalLamp

      // Lamp is unfortunately complicated.  There was only a single lamp type
      // in the old model and the old model would permit a mixed list of any
      // lamp type.  Here we map the old open-ended list to a representative
      // type: Continuum \/ OneAnd[ISet, Arc].
      val Lamp = {
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

        Key[GcalLamp]("lamp") { lamps =>
          val (continuum, arc) = ((List.empty[GcalContinuum], List.empty[GcalArc])/:lamps.split(',')) {
            case ((cs,as), s) =>
              val cs2 = lampToContinuum(s).fold(cs) { _ :: cs }
              val as2 = lampToArc(s).fold(as) { _ :: as }
              (cs2, as2)
          }
          GcalLamp.unsafeFromConfig(continuum.headOption, arc.strengthR(true): _*)
        }
      }

      val Filter = Key.enum[GcalFilter]("filter",
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

      val Diffuser = Key.enum[GcalDiffuser]("diffuser",
        "IR"      -> GcalDiffuser.Ir,
        "visible" -> GcalDiffuser.Visible
      )

      val Shutter = Key.enum[GcalShutter]("shutter",
        "Closed" -> GcalShutter.Closed,
        "Open"   -> GcalShutter.Open
      )

      val ExposureTime = Key[Duration]("exposureTime")(Read.durSecs)
      val Coadds       = Key[Int     ]("coadds"      )(Read.int    )
    }
  }

  implicit class ConfigOps3(cm: ConfigMap) {
    def cget[A](k: System#Key[A]): Option[A] =
      k.legacyGet(cm).map(k.read)

    def uget[A](k: System#Key[A]): A =
      cgetOrElse(k, sys.error(s"not found: ${k.path}"))

    def cgetOrElse[A](k: System#Key[A], a: => A): A =
      cget(k).getOrElse(a)
  }

  def unsafeStep(m: ConfigMap): Step[(InstrumentConfig, Boolean)] = {
    val observeType   = m.uget(Legacy.Observe.ObserveType)
    val instrument    = unsafeInstConfig(m.uget(Legacy.Instrument.Instrument), m)

    val step = observeType match {
      case "BIAS" =>
        BiasStep(instrument)

      case "DARK" =>
        DarkStep(instrument)

      case "OBJECT" | "CAL" =>
        val p = m.cgetOrElse(Legacy.Telescope.P, OffsetP.Zero)
        val q = m.cgetOrElse(Legacy.Telescope.Q, OffsetQ.Zero)
        ScienceStep(instrument, TelescopeConfig(p, q))

      case "ARC" | "FLAT" =>
        val l = m.uget(Legacy.Calibration.Lamp)
        val f = m.uget(Legacy.Calibration.Filter)
        val d = m.uget(Legacy.Calibration.Diffuser)
        val s = m.uget(Legacy.Calibration.Shutter)
        val e = m.uget(Legacy.Calibration.ExposureTime)
        val c = m.uget(Legacy.Calibration.Coadds)
        GcalStep(instrument, GcalConfig(l, f, d, s, e, c))

      case x =>
        sys.error(s"Unexpected observeType: $x${m.toList.mkString("\n>  ", "\n  ", "")}")
    }

    step.strengthR(m.uget(Legacy.Observe.Status))
  }

  def unsafeInstConfig(i: Instrument, m: ConfigMap): InstrumentConfig =
    i match {
      case Instrument.Flamingos2 =>
        val disperser     = m.uget(Legacy.Instrument.F2.Disperser)
        val exposureTime  = m.cgetOrElse(Legacy.Observe.ExposureTime, Duration.ofMillis(0))
        val filter        = m.uget(Legacy.Instrument.F2.Filter)
        val fpu           = m.uget(Legacy.Instrument.F2.Fpu)
        val lyotWheel     = m.uget(Legacy.Instrument.F2.LyotWheel)
        val mosPreimaging = m.uget(Legacy.Instrument.MosPreImaging)
        val readMode      = m.uget(Legacy.Instrument.F2.ReadMode)
        val windowCover   = m.cgetOrElse(Legacy.Instrument.F2.WindowCover, F2WindowCover.Open)
        F2Config(disperser, exposureTime, filter, fpu, lyotWheel, mosPreimaging, readMode, windowCover)

      case _ => GenericConfig(i)
    }
}
