package gem.ocs2

import edu.gemini.spModel.core.{OffsetP, OffsetQ}
import gem._
import gem.config._
import gem.enum._

import gem.ocs2.pio.{PioDecoder, PioError, PioParse}
import gem.ocs2.pio.PioError._

import java.time.Duration

import scala.reflect.runtime.universe.TypeTag
import scala.xml.Node
import scalaz._
import Scalaz._

/** Decoder for the OCS2 sequence XML.
  */
object SequenceDecoder extends PioDecoder[List[Step[DynamicConfig]]] {

  // Clean up classnames in toString, which tells you the type of the key, which should be
  // helpful for debugging.
  private def clean(s: String) =
    s.replace("edu.gemini.spModel.core.", "")
     .replace("java.lang.", "")
     .replace("java.time.", "")
     .replace("gem.", "")

  sealed abstract class System(val system: String) {

    final class Key[A](val name: String, val p: PioParse[A])(implicit ev: TypeTag[A]) {
      val path = s"$system:$name"
      val tpe  = clean(ev.tpe.toString)

      def rawValue(cm: ConfigMap): PioError \/ String =
        cm.lookup(path) \/> missingKey(name)

      def parse(cm: ConfigMap): PioError \/ A =
        rawValue(cm).flatMap { s => p(s) \/> parseError(s, tpe) }

      def cparse(cm: ConfigMap): PioError \/ Option[A] =
        parse(cm) match {
          case -\/(MissingKey(_)) => None.right
          case -\/(err)           => err.left
          case \/-(a)             => Some(a).right
        }

      def cparseOrElse(cm: ConfigMap, a: => A): PioError \/ A =
        cparse(cm).map { _.getOrElse(a) }

      override def toString: String =
        s"Key[$tpe]($path)"
    }

    protected object Key {
      def apply[A: TypeTag](name: String)(parse: PioParse[A]): Key[A] =
        new Key(name, parse)
    }
  }

  object Legacy {
    case object Telescope extends System("telescope") {
      val P = Key("p")(Parsers.offsetP)
      val Q = Key("q")(Parsers.offsetQ)
    }

    case object Observe extends System("observe") {
      val ObserveType  = Key("observeType" )(PioParse.string )
      val ExposureTime = Key("exposureTime")(PioParse.seconds)
    }

    case object Ocs extends System("ocs") {
      val ObservationId = Key("observationId")(Parsers.obsId)
    }

    case object Instrument extends System("instrument") {
      val Instrument    = Key("instrument"   )(Parsers.instrument)
//      val MosPreImaging = Key("mosPreimaging")(Parsers.yesNo     )

      object Flamingos2 {
        val Disperser   = Key("disperser"  )(Parsers.Flamingos2.disperser  )
        val Filter      = Key("filter"     )(Parsers.Flamingos2.filter     )
        val Fpu         = Key("fpu"        )(Parsers.Flamingos2.fpu        )
        val LyotWheel   = Key("lyotWheel"  )(Parsers.Flamingos2.lyotWheel  )
        val ReadMode    = Key("readMode"   )(Parsers.Flamingos2.readMode   )
        val WindowCover = Key("windowCover")(Parsers.Flamingos2.windowCover)
      }
    }

    object Calibration extends System("calibration") {
      val Lamp         = Key("lamp"        )(Parsers.Calibration.lamp    )
      val Filter       = Key("filter"      )(Parsers.Calibration.filter  )
      val Diffuser     = Key("diffuser"    )(Parsers.Calibration.diffuser)
      val Shutter      = Key("shutter"     )(Parsers.Calibration.shutter )
      val ExposureTime = Key("exposureTime")(PioParse.seconds            )
      val Coadds       = Key("coadds"      )(PioParse.int                )
    }
  }

  def parseInstConfig(i: Instrument, cm: ConfigMap): PioError \/ DynamicConfig =
    i match {
      case Instrument.Flamingos2 =>
        import Legacy.Instrument.Flamingos2._
        for {
          d <- Disperser.parse(cm)
          e <- Legacy.Observe.ExposureTime.cparseOrElse(cm, Duration.ofMillis(0))
          f <- Filter.parse(cm)
          u <- Fpu.parse(cm)
          l <- LyotWheel.parse(cm)
//          p <- Legacy.Instrument.MosPreImaging.parse(cm)
          r <- ReadMode.parse(cm)
          w <- WindowCover.cparseOrElse(cm, F2WindowCover.Close)
        } yield F2Config(d, e, f, u, l, r, w)

      case _ => sys.error(s"Can't decode config $i, $cm")
    }

  def parseStep(cm: ConfigMap): PioError \/ Step[DynamicConfig] = {
    def go(observeType: String, instrument: DynamicConfig): PioError \/ Step[DynamicConfig] =
      observeType match {
        case "BIAS" =>
          BiasStep(instrument).right

        case "DARK" =>
          DarkStep(instrument).right

        case "OBJECT" | "CAL" =>
          for {
            p <- Legacy.Telescope.P.cparseOrElse(cm, OffsetP.Zero)
            q <- Legacy.Telescope.Q.cparseOrElse(cm, OffsetQ.Zero)
          } yield ScienceStep(instrument, TelescopeConfig(p, q))

        case "ARC" | "FLAT" =>
          import Legacy.Calibration._
          for {
            l <- Lamp.parse(cm)
            f <- Filter.parse(cm)
            d <- Diffuser.parse(cm)
            s <- Shutter.parse(cm)
            e <- ExposureTime.parse(cm)
            c <- Coadds.parse(cm)
          } yield GcalStep(instrument, GcalConfig(l, f, d, s, e, c.toShort))

        case x =>
          PioError.parseError(x, "ObserveType").left
      }

    for {
      o <- Legacy.Observe.ObserveType.parse(cm)
      i <- Legacy.Instrument.Instrument.parse(cm)
      c <- parseInstConfig(i, cm)
      s <- go(o, c)
    } yield s
  }

  def decode(n: Node): PioError \/ List[Step[DynamicConfig]] = {

    def extractSystem(cm: ConfigMap, sys: Node): ConfigMap = {
      val sysName = (sys \ "@name").text

      (cm /: (sys \ "param")) { (map, param) =>
        val paramName  = (param \ "@name" ).text
        val paramValue = (param \ "@value").text
        map + (s"$sysName:$paramName" -> paramValue)
      }
    }

    def extractStep(cm: ConfigMap, step: Node): ConfigMap =
      (cm /: (step \ "system"))(extractSystem)

    // Extracts the steps in the XML sequence to a simple list of Maps where
    // each Map is all the keys and values that apply to the step.
    (n \ "step").toList.scanLeft(EmptyConfigMap)(extractStep).tail.traverseU(parseStep)
  }
}
