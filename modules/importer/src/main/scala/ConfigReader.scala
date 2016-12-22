package gem

import edu.gemini.spModel.core._
import edu.gemini.spModel.`type`.SequenceableSpType
import edu.gemini.spModel.data.YesNoType

import gem.config._
import gem.config.GcalConfig.GcalLamp
import gem.enum._

import java.time.Duration
import java.util.{Set => JSet}

import scala.collection.JavaConverters._
import scala.reflect.runtime.universe.TypeTag

import edu.gemini.spModel.gemini.calunit.{CalUnitParams => OldGcal}
import edu.gemini.spModel.gemini.flamingos2.{Flamingos2 => OldF2}
import edu.gemini.shared.util.immutable.{Some => GSome}

import scalaz._
import Scalaz._

object ConfigReader {

  type Show[A] = A => String
  type Read[A] = AnyRef => A

  // Clean up classnames in toString, which tells you the type of the key, which should be
   // helpful for debugging.
   private def clean(s: String) =
     s.replace("edu.gemini.spModel.core.", "")
      .replace("java.lang.", "")
      .replace("java.time.", "")
      .replace("gem.", "")

  private def swapMap[A, B](m: Map[A, B]): Map[B, A] =
    m.map(_.swap).toMap

  object Enum {
    def find[A](f: A => Boolean)(implicit ev: Enumerated[A]): Option[A] =
      ev.all.find(f)

    def findp[A: Enumerated, B: Equal](f: A => B)(b: B): Option[A] =
      find[A](a => f(a) === b)

    def ufindp[A: Enumerated, B: Equal](f: A => B)(b: B)(implicit ev: TypeTag[A]): A =
      find[A](a => f(a) === b).getOrElse(sys.error(s"enum value of type ${ev.tpe} not found: $b"))
  }

  // This isn't a typeclass because instances aren't unique
  object Read {
    import Enum._

    def cast[A]: Read[A] =
      _.asInstanceOf[A]

    def enum[A: Enumerated: TypeTag](f: A => String): Read[A] =
      cast[String].map(ufindp(f))

    val unit:          Read[Unit         ] = _ => ()
    val string:        Read[String       ] = cast[String]
    val double:        Read[Double       ] = cast[Double]
    val int:           Read[Int          ] = cast[Int]
    val long:          Read[Long         ] = cast[Long]
    val offsetAngle:   Read[Angle        ] = string.map(s => Angle.fromArcsecs(s.toDouble))
    val yesNo:         Read[Boolean      ] = cast[YesNoType].map(_.toBoolean)
    val durSecs:       Read[Duration     ] = double.map(d => (d * 1000).round).map(Duration.ofMillis(_))
  }


  sealed abstract class System(private val system: String) {

    override def toString: String =
      s"System($system)"

    final class Key[A](val name: String, val write: Show[A], val read: Read[A])(implicit ev: TypeTag[A]) {
      val path = s"$system:$name"

      def legacyGet(c: Map[String, AnyRef]): Option[AnyRef] =
        c.get(path)

      override def toString: String =
        s"Key[${clean(ev.tpe.toString)}]($path)"

      final class Entry private[Key](a: A) {
        val value = a
        val key   = Key.this
        override def toString: String =
          s"Key.Entry[${clean(ev.tpe.toString)}]($path -> ${write(a)})"
      }

      def apply(a: A) = new Entry(a)
    }

    protected object Key {
      def apply[A: TypeTag](name: String, write: Show[A])(read: Read[A]): Key[A] =
        new Key(name, write, read)

      // Produces a pair of functions, one from old to new (S => A) and one
      // from new to old (A => S)
      private def xlat[S <: SequenceableSpType, A](name: String, m: Seq[(S, A)]): (S => A, A => S) = {
        val oldToNew: Map[S, A] = m.toMap.withDefault { o =>
          sys.error(s"new value of '$name': $o not found")
        }
        val newToOld: Map[A, S] = swapMap(oldToNew).withDefault { n =>
          sys.error(s"old value of '$name': $n not found")
        }

        (oldToNew.apply, newToOld.apply)
      }

      def enum[S <: SequenceableSpType, A: TypeTag](name: String, m: (S, A)*): Key[A] =
        enumXlat[S, A](name, m: _*)(_.asInstanceOf[S])

      def enumXlat[S <: SequenceableSpType, A: TypeTag](name: String, m: (S, A)*)(f: AnyRef => S): Key[A] = {
        val (sToA, aToS) = xlat(name, m)
        val write        = aToS.andThen(_.sequenceValue)
        val read         = (any: AnyRef) => sToA(f(any))
        new Key[A](name, write, read)
      }
    }

  }


  object Legacy {

    case object Telescope extends System("telescope") {
      val P = Key[OffsetP]("p", a => f"${a.toAngle.toSignedDegrees * 3600}%4.3f")(Read.offsetAngle.map(OffsetP(_)))
      val Q = Key[OffsetQ]("q", a => f"${a.toAngle.toSignedDegrees * 3600}%4.3f")(Read.offsetAngle.map(OffsetQ(_)))
    }

    case object Observe extends System("observe") {
      val ObserveType   = Key[String  ]("observeType",  identity           )(Read.string)
      val ExposureTime  = Key[Duration]("exposureTime", _.toMillis.toString)(Read.durSecs)
    }

    case object Instrument extends System("instrument") {
      val Instrument    = Key[Instrument]("instrument",    _.shortName                )(Read.enum(_.shortName))
      val MosPreImaging = Key[Boolean   ]("mosPreimaging", b => if (b) "YES" else "NO")(Read.yesNo)

      // Instruments share config fields (all filters go into "instrument:filter"
      // for example) so we repeat them below, name-spaced per instrument for
      // type safety.

      object F2 {

        import F2Disperser._
        val Disperser   = Key.enum[OldF2.Disperser, F2Disperser]("disperser",
          OldF2.Disperser.NONE    -> NoDisperser,
          OldF2.Disperser.R1200HK -> R1200HK,
          OldF2.Disperser.R1200JH -> R1200JH,
          OldF2.Disperser.R3000   -> R3000
        )

        import F2Filter._
        val Filter      = Key.enum[OldF2.Filter, F2Filter]("filter",
          OldF2.Filter.OPEN    -> Open,
          OldF2.Filter.DARK    -> Dark,
          OldF2.Filter.F1056   -> F1056,
          OldF2.Filter.F1063   -> F1063,
          OldF2.Filter.H       -> H,
          OldF2.Filter.HK      -> HK,
          OldF2.Filter.J       -> J,
          OldF2.Filter.J_LOW   -> JLow,
          OldF2.Filter.JH      -> JH,
          OldF2.Filter.K_LONG  -> KLong,
          OldF2.Filter.K_SHORT -> KShort,
          OldF2.Filter.Y       -> Y
        )

        import F2FpUnit._
        val Fpu         = Key.enum[OldF2.FPUnit, F2FpUnit]("fpu",
          OldF2.FPUnit.PINHOLE        -> Pinhole,
          OldF2.FPUnit.SUBPIX_PINHOLE -> SubPixPinhole,
          OldF2.FPUnit.FPU_NONE       -> None,
          OldF2.FPUnit.CUSTOM_MASK    -> Custom,
          OldF2.FPUnit.LONGSLIT_1     -> LongSlit1,
          OldF2.FPUnit.LONGSLIT_2     -> LongSlit2,
          OldF2.FPUnit.LONGSLIT_3     -> LongSlit3,
          OldF2.FPUnit.LONGSLIT_4     -> LongSlit4,
          OldF2.FPUnit.LONGSLIT_6     -> LongSlit6,
          OldF2.FPUnit.LONGSLIT_8     -> LongSlit8
        )

        import F2LyotWheel._
        val LyotWheel   = Key.enum[OldF2.LyotWheel, F2LyotWheel]("lyotWheel",
          OldF2.LyotWheel.GEMS       -> F33Gems,
          OldF2.LyotWheel.GEMS_OVER  -> GemsUnder,
          OldF2.LyotWheel.GEMS_UNDER -> GemsOver,
          OldF2.LyotWheel.H1         -> HartmannA,
          OldF2.LyotWheel.H2         -> HartmannB,
          OldF2.LyotWheel.HIGH       -> F32High,
          OldF2.LyotWheel.LOW        -> F32Low,
          OldF2.LyotWheel.OPEN       -> F16
        )

        // It appears that the window cover is sometimes "bare" and sometimes
        // wrapped in `Some` ... it is not `None` in my test data so there's
        // not a mapping for it.
        val WindowCover = Key.enumXlat[OldF2.WindowCover, F2WindowCover]("windowCover",
          OldF2.WindowCover.CLOSE -> F2WindowCover.Close,
          OldF2.WindowCover.OPEN  -> F2WindowCover.Open
        ) {
          case s: OldF2.WindowCover => s
          case g: GSome[_]          => g.getValue.asInstanceOf[OldF2.WindowCover]
        }
      }
    }

    case object Calibration extends System("calibration") {

      import GcalArc._
      import GcalContinuum._

      // Lamp is unfortunately complicated.  There was only a single lamp type
      // in the old model and the old model would permit a mixed list of any
      // lamp type.  Here we map the old open-ended list to a representative
      // type: Continuum \/ OneAnd[ISet, Arc].
      val Lamp = {
        val lampToContinuum = Map[OldGcal.Lamp, GcalContinuum](
          OldGcal.Lamp.IR_GREY_BODY_HIGH -> IrGreyBodyHigh,
          OldGcal.Lamp.IR_GREY_BODY_LOW  -> IrGreyBodyLow,
          OldGcal.Lamp.QUARTZ            -> QuartzHalogen
        ).withDefault { l =>
          sys.error(s"could not find continuum for lamp $l")
        }

        val continuumToLamp = swapMap(lampToContinuum).withDefault { c =>
          sys.error(s"could not find lamp for continuum $c")
        }

        val lampToArc       = Map[OldGcal.Lamp, GcalArc](
          OldGcal.Lamp.AR_ARC            -> ArArc,
          OldGcal.Lamp.CUAR_ARC          -> CuArArc,
          OldGcal.Lamp.THAR_ARC          -> ThArArc,
          OldGcal.Lamp.XE_ARC            -> XeArc
        ).withDefault { l =>
          sys.error(s"could not find arc for lamp $l")
        }

        val arcToLamp = swapMap(lampToArc).withDefault { a =>
          sys.error(s"could not find lamp for arc $a")
        }

        def write(l: GcalLamp): String =
          l.leftMap(c => List(continuumToLamp(c).sequenceValue)).map { arcs =>
            (arcs.head :: arcs.tail.toList).map(arcToLamp.andThen(_.sequenceValue))
          }.merge.mkString("[", ",", "]")

        Key[GcalLamp]("lamp", write) {
          case js: JSet[_] =>
            val oldLamps     = js.asInstanceOf[JSet[OldGcal.Lamp]].asScala.toList
            val (oldC, oldA) = oldLamps.partition(_.`type` == OldGcal.LampType.flat)
            val newC         = oldC.map(lampToContinuum)
            val newA         = oldA.map(lampToArc)
            GcalConfig.unsafeMkLamp(newC.headOption, newA.strengthR(true): _*)
        }
      }

      val Filter = Key.enum[OldGcal.Filter, GcalFilter]("filter",
        OldGcal.Filter.NONE  -> GcalFilter.None,
        OldGcal.Filter.ND_10 -> GcalFilter.Nd10,
        OldGcal.Filter.ND_16 -> GcalFilter.Nd16,
        OldGcal.Filter.ND_20 -> GcalFilter.Nd20,
        OldGcal.Filter.ND_30 -> GcalFilter.Nd30,
        OldGcal.Filter.ND_40 -> GcalFilter.Nd40,
        OldGcal.Filter.ND_45 -> GcalFilter.Nd45,
        OldGcal.Filter.ND_50 -> GcalFilter.Nd50,
        OldGcal.Filter.GMOS  -> GcalFilter.Gmos,
        OldGcal.Filter.HROS  -> GcalFilter.Hros,
        OldGcal.Filter.NIR   -> GcalFilter.Nir
      )

      val Diffuser = Key.enum[OldGcal.Diffuser, GcalDiffuser]("diffuser",
        OldGcal.Diffuser.IR      -> GcalDiffuser.Ir,
        OldGcal.Diffuser.VISIBLE -> GcalDiffuser.Visible
      )

      val Shutter = Key.enum[OldGcal.Shutter, GcalShutter]("shutter",
        OldGcal.Shutter.CLOSED -> GcalShutter.Closed,
        OldGcal.Shutter.OPEN   -> GcalShutter.Open
      )

      val ExposureTime = Key[Duration]("exposureTime", _.toMillis.toString)(Read.durSecs)
      val Coadds       = Key[Int     ]("coadds",       _.toString)(Read.int)
    }
  }

  implicit class ConfigOps3(c: Map[String, AnyRef]) {
    def cget[A](k: System#Key[A]): Option[A] =
      k.legacyGet(c).map(k.read)

    def uget[A](k: System#Key[A]): A =
      cgetOrElse(k, sys.error(s"not found: ${k.path}"))

    def cgetOrElse[A](k: System#Key[A], a: => A): A =
      cget(k).getOrElse(a)
  }

}
