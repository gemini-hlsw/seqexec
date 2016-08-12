package gem

import edu.gemini.spModel.core._

import scala.reflect.runtime.universe.TypeTag


object Tcc {

  // The idea here is that we define all TCC keys and value -> string mappings here, and provide
  // no way for external callers to get at the TCC's string representation. This will help prevent
  // stringly-typed stuff from leaking.

  private def clean(s: String) =
    s.replace("edu.gemini.spModel.core.", "")
     .replace("java.lang.", "")
     .replace("gem.", "")

  // This might work better flattened
  sealed abstract class System(private[Tcc] val tccSystem: String) {
    override def toString = s"Tcc.System($tccSystem)"
    final class Key[A] private (tccKey: String, tccValue: A => String)(implicit ev: TypeTag[A]) {
      override def toString = s"Tcc.Key[${clean(ev.tpe.toString)}](${tccSystem}:${tccKey})"
      val system = System.this
      final class Entry private[Key](a: A) {
        val value = a
        val key = Key.this
        override def toString = s"Tcc.Entry[${clean(ev.tpe.toString)}](${tccSystem}:${tccKey} -> ${tccValue(a)})"
      }
      def apply(a: A) = new Entry(a)
    }
    protected object Key { 
      def apply[A: TypeTag](tccKey: String)(tccValue: A => String) = new Key(tccKey, tccValue)
    }
  }

  case object Telescope extends System("telescope") {
    val P = Key[OffsetP]("p")(a => f"${a.toAngle.toSignedDegrees * 3600}%4.3f")
    val Q = Key[OffsetQ]("q")(a => f"${a.toAngle.toSignedDegrees * 3600}%4.3f")
  }

  case object Observe extends System("observe") {

  }
  
  case object Instrument extends System("instrument") {
    val Instrument = Key[gem.Instrument]("instrument")(_.name)
  }
  
  case object Calibration extends System("calibration") {

  }

  type Config = List[System#Key[_]#Entry]


}

