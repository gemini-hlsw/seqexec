package gem

import edu.gemini.spModel.core._
import edu.gemini.spModel.`type`.SequenceableSpType
import edu.gemini.spModel.data.YesNoType
import gem.config._
import gem.enum._
import java.time.Duration
import java.util.{ Set => JSet }
import scala.reflect.runtime.universe.TypeTag
import edu.gemini.spModel.gemini.calunit.{ CalUnitParams => OldGCal }
import edu.gemini.spModel.gemini.flamingos2.{ Flamingos2 => OldF2 }

import scalaz._, Scalaz._

object ConfigReader3 {

  object Enum {
    def find[A](f: A => Boolean)(implicit ev: Enumerated[A]): Option[A] =
      ev.all.find(f)

    def findp[A: Enumerated, B: Equal](f: A => B)(b: B): Option[A] =
      find[A](a => f(a) === b)

    def ufindp[A: Enumerated, B: Equal](f: A => B)(b: B)(implicit ev: TypeTag[A]): A =
      find[A](a => f(a) === b).getOrElse(sys.error(s"enum value of type ${ev.tpe} not found: $b"))
  }

  // This isn't a typeclass because instances aren't unique
  type Read[A] = AnyRef => A
  object Read {
    import Enum._

    def cast[A]: Read[A] =
      _.asInstanceOf[A]

    def enum[A: Enumerated: TypeTag](f: A => String): Read[A] =
      cast[String].map(ufindp(f))

    def seq[S <: SequenceableSpType, A: Enumerated: TypeTag](f: A => String): Read[A] =
      cast[S].map(s => ufindp(f)(s.sequenceValue))

    val unit:        Read[Unit       ] = _ => ()
    val string:      Read[String     ] = cast[String]
    val double:      Read[Double     ] = cast[Double]
    val int:         Read[Int        ] = cast[Int]
    val long:        Read[Long       ] = cast[Long]
    val offsetAngle: Read[Angle      ] = string.map(s => Angle.fromArcsecs(s.toDouble))
    val offsetP:     Read[OffsetP    ] = offsetAngle.map(OffsetP(_))
    val offsetQ:     Read[OffsetQ    ] = offsetAngle.map(OffsetQ(_))
    val instrument:  Read[Instrument ] = enum(_.tccValue)
    val yesNo:       Read[Boolean    ] = cast[YesNoType].map(_.toBoolean)
    val durSecs:     Read[Duration   ] = double.map(_.toInt).map(Duration.ofSeconds(_))
    val gcalLamp:    Read[GCalLamp   ] = cast[JSet[Object]].map(_.iterator.next.toString).map(ufindp[GCalLamp, String](_.tccValue))
    val gcalShutter: Read[GCalShutter] = seq[OldGCal.Shutter, GCalShutter](_.tccValue)
    val f2fpu:       Read[F2FpUnit   ] = seq[OldF2.FPUnit,    F2FpUnit   ](_.tccValue)
    val f2filter:    Read[F2Filter   ] = seq[OldF2.Filter,    F2Filter   ](_.tccValue)
    val f2lyotwheel: Read[F2LyotWheel] = seq[OldF2.LyotWheel, F2LyotWheel](_.tccValue)
    val f2disperser: Read[F2Disperser] = seq[OldF2.Disperser, F2Disperser](_.tccValue)

  }

  case class KeyRead[A](k: Tcc.System#Key[A], r: Read[A])

  object Legacy {
    object Telescope {
      val P = KeyRead(Tcc.Telescope.P, Read.offsetP)
      val Q = KeyRead(Tcc.Telescope.Q, Read.offsetQ)
    }
    object Observe {
      val ObserveType  = KeyRead(Tcc.Observe.ObserveType, Read.string)
      val ExposureTime = KeyRead(Tcc.Observe.ExposureTime, Read.durSecs)
    }
    object Instrument {
      val Instrument    = KeyRead(Tcc.Instrument.Instrument,    Read.instrument)
      val MosPreImaging = KeyRead(Tcc.Instrument.MosPreImaging, Read.yesNo)
      object F2 {
        val Fpu       = KeyRead(Tcc.Instrument.F2.Fpu,       Read.f2fpu)
        val Filter    = KeyRead(Tcc.Instrument.F2.Filter,    Read.f2filter)
        val LyotWheel = KeyRead(Tcc.Instrument.F2.LyotWheel, Read.f2lyotwheel)
        val Disperser = KeyRead(Tcc.Instrument.F2.Disperser, Read.f2disperser)
      }
    }
    object Calibration {
      val Lamp    = KeyRead(Tcc.Calibration.Lamp, Read.gcalLamp)
      val Shutter = KeyRead(Tcc.Calibration.Shutter, Read.gcalShutter)
    }
  }

  implicit class ConfigOps3(c: Map[String, AnyRef]) {
    def cget[A](kr: KeyRead[A]): Option[A] = kr.k.legacyGet(c).map(kr.r)
    def uget[A](k: KeyRead[A]): A = cgetOrElse(k, throw sys.error(s"not found: ${k.k}"))
    def cgetOrElse[A](k: KeyRead[A], a: => A): A = cget(k).getOrElse(a)
  }

}
