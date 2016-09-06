package gem

import gem.enum._
import edu.gemini.spModel.core._
import java.time.Duration
import scala.reflect.runtime.universe.TypeTag

object Tcc {

  // The idea here is that we define all TCC keys and value -> string mappings here, and provide
  // no way for external callers to get at the TCC's string representation. This will help prevent
  // stringly-typed stuff from leaking.

  // Clean up classnames in toString, which tells you the type of the key, which should be
  // helpful for debugging.
  private def clean(s: String) =
    s.replace("edu.gemini.spModel.core.", "")
     .replace("java.lang.", "")
     .replace("java.time.", "")
     .replace("gem.", "")

  // This might work better flattened
  sealed abstract class System(private[Tcc] val tccSystem: String) {
    override def toString = s"Tcc.System($tccSystem)"
    final class Key[A] private (tccKey: String, tccValue: A => String)(implicit ev: TypeTag[A]) {
      private[Tcc] val path = s"${tccSystem}:${tccKey}"
      def legacyGet(c: Map[String, AnyRef]): Option[AnyRef] = c.get(path)
      override def toString = s"Tcc.Key[${clean(ev.tpe.toString)}]($path)"
      val system = System.this
      final class Entry private[Key](a: A) {
        val value = a
        val key = Key.this
        override def toString = s"Tcc.Entry[${clean(ev.tpe.toString)}]($path -> ${tccValue(a)})"
      }
      def apply(a: A) = new Entry(a)
    }
    protected object Key {
      def apply[A: TypeTag](tccKey: String)(tccValue: A => String) = new Key(tccKey, tccValue)
    }
  }

  // This is the only place where TCC keys are defined, and the string "path" values cannot
  // escape this module. From our point of view they are pure typed values.

  case object Telescope extends System("telescope") {
    val P = Key[OffsetP]("p")(a => f"${a.toAngle.toSignedDegrees * 3600}%4.3f")
    val Q = Key[OffsetQ]("q")(a => f"${a.toAngle.toSignedDegrees * 3600}%4.3f")
  }

  case object Observe extends System("observe") {
    val ObserveType  = Key[String  ]("observeType" )(identity) // TODO: reconcile with stepType
    val ExposureTime = Key[Duration]("exposureTime")(_.getSeconds.toString) // TODO: check units
  }

  case object Instrument extends System("instrument") {

    val Instrument    = Key[Instrument]("instrument"   )(_.tccValue)
    val MosPreImaging = Key[Boolean   ]("mosPreimaging")(b => if(b) "YES" else "NO") // wtaf

    // Instruments share config fields (all filters go into "instrument:filter" for example) so
    // we repeat them below, namespaced per instrument for type safety.

    object F2 {
      val Fpu       = Key[F2FpUnit   ]("fpu"      )(_.tccValue)
      val Filter    = Key[F2Filter   ]("filter"   )(_.tccValue)
      val LyotWheel = Key[F2LyotWheel]("lyotWheel")(_.tccValue)
      val Disperser = Key[F2Disperser]("disperser")(_.tccValue)
    }

  }

  case object Calibration extends System("calibration") {
    val Lamp    = Key[GCalLamp   ]("lamp"   )(_.tccValue)
    val Shutter = Key[GCalShutter]("shutter")(_.tccValue)
  }

}

trait TccTypes {


}
