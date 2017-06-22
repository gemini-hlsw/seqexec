// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import gem.ocs2.pio.PioError._
import gem.ocs2.pio.{PioError, PioParse}

import scala.reflect.runtime.universe.TypeTag

import scalaz._
import Scalaz._


/** Legacy system (telescope, instrument, observe, calibration) key and parser
  * definitions.
  */
@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object Legacy {
  sealed abstract class System(val system: String) {

    final class Key[A](val name: String, val p: PioParse[A])(implicit ev: TypeTag[A]) {

      val path: String = s"$system:$name"

      @SuppressWarnings(Array("org.wartremover.warts.ToString"))
      val tpe:  String = Key.clean(ev.tpe.toString)

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
      // Clean up classnames in toString, which tells you the type of the key, which should be
      // helpful for debugging.
      private def clean(s: String) =
        s.replace("edu.gemini.spModel.core.", "")
         .replace("java.lang.", "")
         .replace("java.time.", "")
         .replace("gem.", "")

      def apply[A: TypeTag](name: String)(parse: PioParse[A]): Key[A] =
        new Key(name, parse)
    }
  }

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
    val MosPreImaging = Key("mosPreimaging")(Parsers.yesNo     )

    object Flamingos2 {
      val Disperser   = Key("disperser"  )(Parsers.Flamingos2.disperser  )
      val Filter      = Key("filter"     )(Parsers.Flamingos2.filter     )
      val Fpu         = Key("fpu"        )(Parsers.Flamingos2.fpu        )
      val LyotWheel   = Key("lyotWheel"  )(Parsers.Flamingos2.lyotWheel  )
      val ReadMode    = Key("readMode"   )(Parsers.Flamingos2.readMode   )
      val WindowCover = Key("windowCover")(Parsers.Flamingos2.windowCover)
    }

    object Gmos {
      val Adc             = Key("adc"                 )(Parsers.Gmos.adc            )
      val AmpCount        = Key("ampCount"            )(Parsers.Gmos.ampCount       )
      val AmpGain         = Key("gainChoice"          )(Parsers.Gmos.ampGain        )
      val AmpReadMode     = Key("ampReadMode"         )(Parsers.Gmos.ampReadMode    )
      val BuiltinRoi      = Key("builtinROI"          )(Parsers.Gmos.builtinRoi     )
      val CustomSlitWidth = Key("customSlitWidth"     )(Parsers.Gmos.customSlitWidth)
      val Detector        = Key("detectorManufacturer")(Parsers.Gmos.detector       )
      val DisperserOrder  = Key("disperserOrder"      )(Parsers.Gmos.disperserOrder )
      val Dtax            = Key("dtaXOffset"          )(Parsers.Gmos.dtax           )
      val XBinning        = Key("ccdXBinning"         )(Parsers.Gmos.binning        )
      val YBinning        = Key("ccdYBinning"         )(Parsers.Gmos.binning        )
    }

    object GmosNorth {
      val Disperser       = Key("disperser")(Parsers.GmosNorth.disperser )
      val Filter          = Key("filter"   )(Parsers.GmosNorth.filter    )
      val Fpu             = Key("fpu"      )(Parsers.GmosNorth.fpu       )
      val StageMode       = Key("stageMode")(Parsers.GmosNorth.stageMode )
    }

    object GmosSouth {
      val Disperser       = Key("disperser")(Parsers.GmosSouth.disperser )
      val Filter          = Key("filter"   )(Parsers.GmosSouth.filter    )
      val Fpu             = Key("fpu"      )(Parsers.GmosSouth.fpu       )
      val StageMode       = Key("stageMode")(Parsers.GmosSouth.stageMode )
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
